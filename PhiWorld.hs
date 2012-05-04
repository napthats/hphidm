module PhiWorld
       (
         ActionResult(..),
         PcStatusChangeType(..),
         NpcStatusChangeType(..),
         ClientIDSet,
         PcSet,
         PhiWorld(),
         Phirc,
         getPhiMap,
         getClientIDSet,
         getPcSet,
         getNpcSet,
         resolveActionResult,
         makePhiWorld,
       ) where

import Data.List (find)
import Data.List.Utils (addToAL, delFromAL)
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC (PlayerCharacter, getPhirc)
import qualified PlayerCharacterDB as PCD
import qualified Chara as CH
import qualified NonPlayerCharacter as NPC
import qualified PhiMap as PM
import qualified DmMessages as DM
import qualified ProtocolEncoder as PE
import qualified Combat as CO


type Phirc = String
-- some of them triger other actions
-- for example, PcStatusChange about pos or dir cause othre pc's MessageFromDm about around view
data ActionResult = 
  NewPc NS.ClientID Phirc PC.PlayerCharacter
  | PcStatusChange PcStatusChangeType PC.PlayerCharacter
  | NpcStatusChange NpcStatusChangeType NPC.NonPlayerCharacter
  | MessageFromDm NS.ClientID String
  | MessageFromPc PC.PlayerCharacter PC.PlayerCharacter String
  | LogoutPc NS.ClientID
  | PcHit PC.PlayerCharacter
  | ForceDisconnect NS.ClientID
data PcStatusChangeType = PSCDirection | PSCPosition deriving (Show)
data NpcStatusChangeType = NPSCDirection | NPSCPosition | NPSCLivetime deriving (Show)

data TriggeredEvernt = Tentative deriving (Show)


newtype PhiWorld = PhiWorld (PM.PhiMap, ClientIDSet, PcSet, NpcSet)

getPhiMap :: PhiWorld -> PM.PhiMap
getPhiMap (PhiWorld (phimap, _, _, _)) = phimap

getClientIDSet :: PhiWorld -> ClientIDSet
getClientIDSet (PhiWorld (_, cidset, _, _)) = cidset

getPcSet :: PhiWorld -> PcSet
getPcSet (PhiWorld (_, _, pcset, _)) = pcset

getNpcSet :: PhiWorld -> NpcSet
getNpcSet (PhiWorld (_, _, _, npcset)) = npcset

type ClientIDSet = [(NS.ClientID, Phirc)]
type PcSet = [(Phirc, PC.PlayerCharacter)]
-- fst NpcSet is newest NpcId for next npc
type NpcSet = (NPC.NpcId, [(NPC.NpcId, NPC.NonPlayerCharacter)])

reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookUp value al = case find ((== value) . snd) al of
  Nothing -> Nothing
  Just (a, _) -> Just a


-- tentative
makePhiWorld :: PhiWorld
makePhiWorld = 
  let phimap = PM.makePhiMap in
  let nid = NPC.newNpcId in
  PhiWorld (phimap, [], [],
            (nid,
             [(nid,NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 nid),
              (NPC.nextNpcId nid,NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc2" 5000 (NPC.nextNpcId nid))]))


resolveActionResult ::
  NS.SimpleTCPServer -> [ActionResult] -> PhiWorld -> PCD.PlayerCharacterDB ->
  IO (PhiWorld, PCD.PlayerCharacterDB)
resolveActionResult server result_list phiworld first_pcdb = do
  let (next_world, next_pcdb, io_list, _, _) =
        foldl _resolveActionResult (phiworld, first_pcdb, [], [], server) result_list
  mapM_ (\x -> do _ <- x; return ()) $ reverse io_list
  return (next_world, next_pcdb)

_resolveActionResult ::
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [TriggeredEvernt], NS.SimpleTCPServer) ->ActionResult ->
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [TriggeredEvernt], NS.SimpleTCPServer)
_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server) result =
  case result of
    NewPc cid phirc pc ->
      let new_cidset = addToAL cidset cid phirc in
      let new_pcset = addToAL pcset phirc pc in
      let pos = CH.getPosition pc in
      let new_io_list = 
            reverse $
            sendLookMessagesToCanSeePosPc server phimap pos new_cidset new_pcset
            (map snd new_pcset) (map snd (snd npcset)) in
      (PhiWorld (phimap, new_cidset, new_pcset, npcset), pcdb,
                 new_io_list ++ io_list, event_list, server)
    PcStatusChange sctype pc ->
      let phirc = PC.getPhirc pc in
      let new_pcset = addToAL pcset phirc pc in
      let pos = CH.getPosition pc in
      case sctype of
        PSCDirection ->
          let new_io_list =
                reverse $
                sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                (map snd new_pcset) (map snd (snd npcset)) in
          (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
                     new_io_list ++ io_list, event_list, server)
        PSCPosition ->
          let new_io_list =
                reverse $
                sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                (map snd new_pcset) (map snd (snd npcset)) in
          (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
           new_io_list ++ io_list, event_list, server)          
    PcHit pc ->
      let hitrange = CH.getHitRange phimap pc in
      let target_pc_list = filter (\tpc -> any (== CH.getPosition tpc) hitrange) (map snd pcset) in
      let target_npc_list =
            filter (\tnpc -> any (== CH.getPosition tnpc) hitrange) (map snd (snd npcset)) in
      let (next_pc, final_target_pc_combat_list) =
            foldl (\(cur_pc, vspc_list) cur_vspc -> 
                    let (_next_pc, next_vspc, combat_result) = CH.hitTo cur_pc cur_vspc in
                    (_next_pc, (next_vspc, combat_result) : vspc_list)
                  ) (pc, []) target_pc_list in
      let (final_pc, final_target_npc_combat_list) =
            foldl (\(cur_pc, vsnpc_list) cur_vsnpc -> 
                    let (_next_pc, next_vsnpc, combat_result) = CH.hitTo cur_pc cur_vsnpc in
                    (_next_pc, (next_vsnpc, combat_result) : vsnpc_list)
                  ) (next_pc, []) target_npc_list in
      let new_pcset = foldl (\cur_pcset new_pc -> addToAL cur_pcset (PC.getPhirc new_pc) new_pc)
                            pcset (final_pc : map fst final_target_pc_combat_list) in
      let new_npcset_snd =
            foldl (\cur_npcset new_npc -> addToAL cur_npcset (NPC.getNpcId new_npc) new_npc)
            (snd npcset) (map fst final_target_npc_combat_list) in
      let new_npcset = (fst npcset, new_npcset_snd) in
      let cid = case reverseLookUp (PC.getPhirc pc) cidset of
            Nothing -> error "Assertion error: pc doesn't have client"         
            Just x -> x in
      let new_io_list_pc = 
            concat $ map (\cresult ->
                           map (\dm_type -> NS.sendMessageTo server cid $ DM.makeDmMessage dm_type)
                           (CO.makeDmMessageTypeList cresult))
            $ (map snd final_target_pc_combat_list) ++ (map snd final_target_npc_combat_list) in
      let new_io_list_target =
            concat $ map (\(tpc, cresult) ->
                           let tcid = case reverseLookUp (PC.getPhirc tpc) cidset of
                                 Nothing -> error "Assertion error: pc doesn't have client"         
                                 Just x -> x in
                           map (\dm_type -> NS.sendMessageTo server tcid $ DM.makeDmMessage dm_type)
                           (CO.makeDmMessageTypeList cresult))
            $ final_target_pc_combat_list in
      (PhiWorld (phimap, cidset, new_pcset, new_npcset), pcdb,
       new_io_list_pc ++ new_io_list_target ++ io_list, event_list, server)                
    NpcStatusChange sctype npc ->
      let nid = NPC.getNpcId npc in
      let new_npcset = (fst npcset, addToAL (snd npcset) nid npc) in
      let pos = CH.getPosition npc in
      case sctype of
        NPSCDirection ->
          let new_io_list =
                reverse $
                sendLookMessagesToCanSeePosPc server phimap pos cidset pcset
                (map snd pcset) (map snd (snd new_npcset)) in
          (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb,
                     new_io_list ++ io_list, event_list, server)
        NPSCPosition ->
          let new_io_list =
                reverse $
                sendLookMessagesToCanSeePosPc server phimap pos cidset pcset
                (map snd pcset) (map snd (snd new_npcset)) in
          (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb,
                     new_io_list ++ io_list, event_list, server)
        NPSCLivetime ->
          (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb, io_list, event_list, server)
    MessageFromDm cid msg ->
      -- ignore disconnected client
      let io = NS.sendMessageTo server cid msg in
      (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server)
    MessageFromPc opc dpc msg ->
      case reverseLookUp (PC.getPhirc dpc) cidset of
        Nothing -> error "Assertion error: pc doesn't have client."
        Just dcid ->
          let io = NS.sendMessageTo server dcid $
                   DM.makeDmMessage (DM.PcMessage (CH.getName opc) msg)
          in (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server)
    LogoutPc cid ->
      case lookup cid cidset of
        -- already disconnected
        Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server)
        Just phirc ->
          let io = NS.disconnectClient server cid in
          let maybe_pc = lookup phirc pcset in
          case maybe_pc of
            Nothing -> error "Assertion error: client don't have pc."
            Just pc -> 
              let new_pcdb = PCD.savePc pcdb phirc pc in
              let new_cidset = delFromAL cidset cid in
              let new_pcset = delFromAL pcset phirc in
              let pos = CH.getPosition pc in
              let new_io_list =
                    reverse $ sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                              (map snd new_pcset) (map snd (snd npcset)) in
              (PhiWorld (phimap, new_cidset, new_pcset, npcset), new_pcdb,
                         new_io_list ++ io : io_list, event_list, server)
    ForceDisconnect cid ->
      let io = NS.disconnectClient server cid in
      (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server)


sendLookMessagesToCanSeePosPc ::
  (CH.Chara a, CH.Chara b) => NS.SimpleTCPServer -> PM.PhiMap -> PM.Position -> ClientIDSet -> PcSet
  -> [a] -> [b] -> [IO Bool]
sendLookMessagesToCanSeePosPc server phimap pos cidset pcset charaset charaset2 = do
  let pc_list = filter (CH.canSee phimap pos) (map snd pcset)
  let cid_pc_list = map (\pc -> (case reverseLookUp (PC.getPhirc pc) cidset of
                                    Nothing -> error "Assertion error: pc doesn't have client"
                                    Just cid -> cid
                                 , pc)) pc_list
  concat $ map (\cid_pc ->
               map (NS.sendMessageTo server (fst cid_pc)) 
               $ [makeAroundView phimap (snd cid_pc)] ++ 
               (makeLookResult phimap (snd cid_pc) charaset) ++
               (makeLookResult phimap (snd cid_pc) charaset2) ++
               [PE.encodeProtocol PE.M57End]
               ) cid_pc_list

makeLookResult ::
  (CH.Chara a) => PM.PhiMap -> PC.PlayerCharacter -> [a] -> [String]
makeLookResult phimap pc charaset = makeAroundCharaView phimap pc charaset

makeAroundCharaView :: (CH.Chara a) => PM.PhiMap -> PC.PlayerCharacter -> [a] -> [String]
makeAroundCharaView phimap pc charaset =
  map (\(CH.CharaView x y rdir name) -> PE.encodeProtocol $ PE.M57Obj PE.CObj x y rdir name)
  (map (CH.getCharaView $ CH.getDirection pc) (CH.getCharaInRegion (CH.getSight phimap pc) charaset))

makeAroundView :: PM.PhiMap -> PC.PlayerCharacter -> String
makeAroundView phimap pc =
  PE.encodeProtocol $ PE.M57Map (CH.getDirection pc) $
  PM.getMapView PM.All phimap (CH.getPosition pc) (CH.getDirection pc) sightWidth sightHeight

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
