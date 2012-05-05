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
         addLivetimeAllNpc,
       ) where

import Data.List (find)
import Data.List.Utils (addToAL, delFromAL)
import qualified Data.Map as Map
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
  | PcStatusChange PcStatusChangeType Phirc (PC.PlayerCharacter -> Maybe PC.PlayerCharacter)
  | NpcStatusChange NpcStatusChangeType NPC.NpcId (NPC.NonPlayerCharacter -> Maybe NPC.NonPlayerCharacter)
  | MessageFromDm NS.ClientID String
  | MessageFromPc PC.PlayerCharacter PC.PlayerCharacter String
  | LogoutPc NS.ClientID
  | PcHit PC.PlayerCharacter
  | ForceDisconnect NS.ClientID
data PcStatusChangeType = PSCDirection | PSCPosition deriving (Show)
data NpcStatusChangeType = NPSCDirection | NPSCPosition deriving (Show)

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
type PcSet = Map.Map Phirc PC.PlayerCharacter
-- fst NpcSet is newest NpcId for next npc
type NpcSet = (NPC.NpcId, Map.Map NPC.NpcId NPC.NonPlayerCharacter)

reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookUp value al = case find ((== value) . snd) al of
  Nothing -> Nothing
  Just (a, _) -> Just a


-- tentative
makePhiWorld :: PhiWorld
makePhiWorld = 
  let phimap = PM.makePhiMap in
  let nid = NPC.newNpcId in
  PhiWorld (phimap, [], Map.empty, (nid, Map.fromList [(nid,NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 nid 1000 1000 1000 1000), (NPC.nextNpcId nid,NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc2" 100000 (NPC.nextNpcId nid) 2000 101 2000 100)]))
--  PhiWorld (phimap, [], Map.empty, (nid, Map.fromList $ take 10000 $ iterate (\(cnid, _) -> (NPC.nextNpcId cnid, NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 (NPC.nextNpcId cnid) 1000 1000 1000 1000)) (nid, NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 nid 1000 1000 1000 1000)))


resolveActionResult ::
  NS.SimpleTCPServer -> [ActionResult] -> PhiWorld -> PCD.PlayerCharacterDB ->
  IO (PhiWorld, PCD.PlayerCharacterDB)
resolveActionResult server result_list phiworld pcdb = do
  let (next_world, next_pcdb, io_list, _, _) =
        foldl _resolveActionResult (phiworld, pcdb, [], [], server) $ result_list
  let (final_world, final_pcdb, io_list_dead, _) = charaDeadCheck server next_world next_pcdb
  mapM_ (\x -> do _ <- x; return ()) $
    (reverse io_list) ++ (reverse io_list_dead)
  return (final_world, final_pcdb)


_resolveActionResult ::
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [TriggeredEvernt], NS.SimpleTCPServer) ->ActionResult ->
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [TriggeredEvernt], NS.SimpleTCPServer)
_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server) result =
  case result of
    NewPc cid phirc pc ->
      let new_cidset = addToAL cidset cid phirc in
      let new_pcset = Map.insert phirc pc pcset in
      let pos = CH.getPosition pc in
      let new_io_list = 
            reverse $
            sendLookMessagesToCanSeePosPc server phimap pos new_cidset new_pcset
            (Map.elems new_pcset) (Map.elems (snd npcset)) in
      (PhiWorld (phimap, new_cidset, new_pcset, npcset), pcdb,
                 new_io_list ++ io_list, event_list, server)
    PcStatusChange sctype phirc pc_change ->
      case Map.lookup phirc pcset of
        Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server)
        Just pc ->
          case pc_change pc of
            Nothing ->              
              case reverseLookUp phirc cidset of
                Nothing -> error "Assertion error (_resolveActionResult): pc doesn't have client."
                Just cid ->
                  let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.GoNo in
                  (PhiWorld (phimap, cidset, pcset, npcset), pcdb, new_io:io_list, event_list, server)
            Just new_pc ->
              let new_pcset = Map.insert phirc new_pc pcset in
              case sctype of
                PSCDirection ->
                  let pos = CH.getPosition new_pc in
                  let new_io_list =
                        reverse $
                        sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                        (Map.elems new_pcset) (Map.elems (snd npcset)) in
                  (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
                   new_io_list ++ io_list, event_list, server)
                PSCPosition ->
                  let pos = CH.getPosition new_pc in
                  let new_io_list =
                        reverse $
                        sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                        (Map.elems new_pcset) (Map.elems (snd npcset)) in
                  (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
                   new_io_list ++ io_list, event_list, server)          
    NpcStatusChange sctype nid npc_change ->
      case Map.lookup nid (snd npcset) of
        Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server)
        Just npc ->
          case npc_change npc of
            Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server)
            Just new_npc ->
              let new_npcset = (fst npcset, Map.insert nid new_npc (snd npcset)) in
              case sctype of
                NPSCDirection ->
                  let pos = CH.getPosition new_npc in
                  let new_io_list =
                        reverse $
                        sendLookMessagesToCanSeePosPc server phimap pos cidset pcset
                        (Map.elems pcset) (Map.elems (snd new_npcset)) in
                  (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb,
                   new_io_list ++ io_list, event_list, server)
                NPSCPosition ->
                  let pos = CH.getPosition new_npc in
                  let new_io_list =
                        reverse $
                        sendLookMessagesToCanSeePosPc server phimap pos cidset pcset
                        (Map.elems pcset) (Map.elems (snd new_npcset)) in
                  (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb,
                   new_io_list ++ io_list, event_list, server)
    PcHit pc ->
      let hitrange = CH.getHitRange phimap pc in
      let target_pc_list = filter (\tpc -> any (== CH.getPosition tpc) hitrange) (Map.elems pcset) in
      let target_npc_list =
            filter (\tnpc -> any (== CH.getPosition tnpc) hitrange) (Map.elems (snd npcset)) in
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
      let new_pcset = foldl (\cur_pcset new_pc -> Map.insert (PC.getPhirc new_pc) new_pc cur_pcset)
                            pcset (final_pc : map fst final_target_pc_combat_list) in
      let new_npcset_snd =
            foldl (\cur_npcset new_npc -> Map.insert (NPC.getNpcId new_npc) new_npc cur_npcset)
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
          let maybe_pc = Map.lookup phirc pcset in
          case maybe_pc of
            Nothing -> error "Assertion error: client don't have pc."
            Just pc -> 
              let new_pcdb = PCD.savePc pcdb phirc pc in
              let new_cidset = delFromAL cidset cid in
              let new_pcset = Map.delete phirc pcset in
              let pos = CH.getPosition pc in
              let new_io_list =
                    reverse $ sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                              (Map.elems new_pcset) (Map.elems (snd npcset)) in
              (PhiWorld (phimap, new_cidset, new_pcset, npcset), new_pcdb,
                         io : new_io_list ++ io_list, event_list, server)
    ForceDisconnect cid ->
      let io = NS.disconnectClient server cid in
      (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server)


sendLookMessagesToCanSeePosPc ::
  (CH.Chara a, CH.Chara b) => NS.SimpleTCPServer -> PM.PhiMap -> PM.Position -> ClientIDSet -> PcSet
  -> [a] -> [b] -> [IO Bool]
sendLookMessagesToCanSeePosPc server phimap pos cidset pcset charaset charaset2 = do
  let pc_list = filter (CH.canSee phimap pos) (Map.elems pcset)
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


charaDeadCheck ::
  NS.SimpleTCPServer -> PhiWorld -> PCD.PlayerCharacterDB ->
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [TriggeredEvernt])
charaDeadCheck server (PhiWorld (phimap, cidset, pcset, npcset)) pcdb =
  let (next_cidset, next_pcset, next_pcdb, io_list_pc, _) =
        foldl pcDeadCheck (cidset, pcset, pcdb, [], server) (Map.elems pcset) in
  let (PhiWorld (_, _, _, next_npcset), io_list_npc, _) =
        foldl npcDeadCheck
          (PhiWorld (phimap, next_cidset, next_pcset, npcset), [], server) (Map.elems $ snd npcset) in
  (PhiWorld (phimap, next_cidset, next_pcset, next_npcset), next_pcdb, io_list_pc ++ io_list_npc, [])

npcDeadCheck :: (PhiWorld, [IO Bool], NS.SimpleTCPServer) -> NPC.NonPlayerCharacter->
                (PhiWorld, [IO Bool], NS.SimpleTCPServer)
npcDeadCheck (PhiWorld (phimap, cidset, pcset, npcset), io_list, server) npc =
  if CH.isDead npc
     then let lastInjuredName = case CH.getLastInjured npc of
                Nothing -> "Assertion error (npcDeadCheck): dead npc doesn't have last injured."
                Just (CH.IBPc ibpc) -> CH.getName ibpc
                Just (CH.IBNpc ibnpc) -> CH.getName ibnpc in
          let cansee_pc_list = filter (CH.canSee phimap (CH.getPosition npc)) (Map.elems pcset) in
          let new_io_list = map (\cspc ->
                                  let cid = case reverseLookUp (PC.getPhirc cspc) cidset of
                                        Nothing ->
                                         error "Assertion error (npcDeadCheck): pc doesn't hvae client"
                                        Just x -> x in
                                  NS.sendMessageTo server cid $ DM.makeDmMessage $
                                  DM.KillBy (CH.getName npc) lastInjuredName)
                            cansee_pc_list in
          let next_npcset = (fst npcset, Map.delete (NPC.getNpcId npc) (snd npcset)) in
          (PhiWorld (phimap, cidset, pcset, next_npcset), new_io_list ++ io_list, server)
     else (PhiWorld (phimap, cidset, pcset, npcset), io_list, server)

pcDeadCheck :: (ClientIDSet, PcSet, PCD.PlayerCharacterDB, [IO Bool], NS.SimpleTCPServer) ->
               PC.PlayerCharacter ->
               (ClientIDSet, PcSet, PCD.PlayerCharacterDB, [IO Bool], NS.SimpleTCPServer)
pcDeadCheck (cidset, pcset, pcdb, io_list, server) pc =
  if CH.isDead pc
     then let phirc = PC.getPhirc pc in
          let cid = case reverseLookUp phirc cidset of
                Nothing -> error "Assertion error (pcDeadCheck): pc doesn't have client"
                Just x -> x in
          let new_io_list = [NS.disconnectClient server cid,
                             NS.sendMessageTo server cid $ PE.encodeProtocol PE.Close,
                             NS.sendMessageTo server cid $ DM.makeDmMessage DM.Savedata,
                             NS.sendMessageTo server cid $ DM.makeDmMessage DM.Tryagain,
                             NS.sendMessageTo server cid $ DM.makeDmMessage DM.Dead] in
          let next_cidset = delFromAL cidset cid in
          let next_pcset = Map.delete phirc pcset in
          let next_pcdb = PCD.savePc pcdb phirc pc in
          (next_cidset, next_pcset, next_pcdb, new_io_list ++ io_list, server)
     else (cidset, pcset, pcdb, io_list, server)


addLivetimeAllNpc :: Int -> PhiWorld -> PhiWorld
addLivetimeAllNpc dtime (PhiWorld (phimap, cidset, pcset, npcset)) =
  PhiWorld (phimap, cidset, pcset, (fst npcset, Map.map (\npc -> NPC.addLivetime dtime npc) (snd npcset)))
