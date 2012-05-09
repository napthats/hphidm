module PhiWorld
       (
         ActionResult(..),
         PcStatusChangeType(..),
         NpcStatusChangeType(..),
         ClientIDSet,
         PcSet,
         PhiWorld(),
         Phirc,
         CharaInstance(..),
         getPhiMap,
         getClientIDSet,
         getPcSet,
         getNpcSet,
         resolveActionResult,
         makePhiWorld,
         addLivetimeAllNpc,
       ) where

import Data.List (find, findIndex)
import Data.Maybe (fromJust)
import Data.List.Utils (addToAL, delFromAL)
import qualified Data.Map as Map
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC
import qualified PlayerCharacterDB as PCD
import qualified Chara as CH
import qualified NonPlayerCharacter as NPC
import qualified PhiMap as PM
import qualified DmMessages as DM
import qualified ProtocolEncoder as PE
import qualified Combat as CO
import qualified Event as EV
import qualified Item as IT
import PhiWorldData


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
  PhiWorld (phimap, [], Map.empty, (nid, Map.fromList []))
--  PhiWorld (phimap, [], Map.empty, (nid, Map.fromList [(nid,NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 nid 1000 1000 1000 1000), (NPC.nextNpcId nid,NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc2" 100000 (NPC.nextNpcId nid) 2000 101 2000 100)]))
--  PhiWorld (phimap, [], Map.empty, (nid, Map.fromList $ take 10000 $ iterate (\(cnid, _) -> (NPC.nextNpcId cnid, NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 (NPC.nextNpcId cnid) 1000 1000 1000 1000)) (nid, NPC.makeNonPlayerCharacter (PM.getDefaultPosition phimap) PM.East "npc1" 1000 nid 1000 1000 1000 1000)))

-- resolve ActionResult from left to right
resolveActionResult ::
  NS.SimpleTCPServer -> [ActionResult] -> PhiWorld -> PCD.PlayerCharacterDB -> EV.SwitchDB ->
  IO (PhiWorld, PCD.PlayerCharacterDB, [EV.TriggeredEvent])
resolveActionResult server result_list phiworld pcdb swdb = do
  let (next_world, next_pcdb, io_list, event_list, _, _) =
        foldl _resolveActionResult (phiworld, pcdb, [], [], server, swdb) $ result_list
  let (final_world, final_pcdb, io_list_dead, _) = charaDeadCheck server next_world next_pcdb swdb
  mapM_ (\x -> do _ <- x; return ()) $
    (reverse io_list) ++ (reverse io_list_dead)
  return (final_world, final_pcdb, event_list)


_resolveActionResult ::
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [EV.TriggeredEvent], NS.SimpleTCPServer, EV.SwitchDB) ->
  ActionResult ->
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [EV.TriggeredEvent], NS.SimpleTCPServer, EV.SwitchDB)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (GetItem chara_instance maybe_item_name) =
  case chara_instance of
    Pc pc cid ->
      let pos = CH.getPosition pc in
      let floor_item_list = PM.getItemList phimap pos in
      case maybe_item_name of
        Nothing ->
          if null floor_item_list
             then let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.NoItemHere in
                  (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                   new_io : io_list, event_list, server, swdb)
             else let new_pc = case PC.setState (PC.SelectList PC.SLGet) pc of
                        Nothing -> pc
                        Just x -> x in
                  let new_pcset = Map.insert (PC.getPhirc pc) new_pc pcset in
                  let new_io_itemlist = NS.sendMessageTo server cid $
                                        DM.makeDmMessage $ DM.List (map IT.getName floor_item_list) in
                  let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.GetSelect in
                  (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
                   new_io : new_io_itemlist : io_list, event_list, server, swdb)
        Just item_name_or_ord ->
          case reads item_name_or_ord :: [(Int, String)] of
            [(item_ord_plus1, "")] ->
              let item_ord = item_ord_plus1 - 1 in
              getItemWithOrd
              $ if length floor_item_list <= item_ord || item_ord < 0 then Nothing else Just item_ord
            _ ->
              getItemWithOrd 
              $ findIndex (\item -> IT.getName item == item_name_or_ord) floor_item_list
            where getItemWithOrd maybe_ord =
                    case maybe_ord of
                      Nothing ->
                        let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.GetBad in
                        (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                         new_io : io_list, event_list, server, swdb)
                      Just item_ord ->
                        let (new_phimap, item) = fromJust $ PM.deleteItem pos item_ord phimap in
                        let new_pc = CH.addItem item pc in
                        let new_pcset = Map.insert (PC.getPhirc pc) new_pc pcset in
                        let new_io = NS.sendMessageTo server cid $
                                     DM.makeDmMessage $ DM.Get (CH.getName pc) (IT.getName item) in
                        (PhiWorld (new_phimap, cidset, new_pcset, npcset), pcdb,
                         new_io : io_list, event_list, server, swdb)
    Npc _ ->
      undefined

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (PutItem chara_instance maybe_item_name) =
  case chara_instance of
    Pc pc cid ->
      let pc_item_list = CH.getItemList pc in
      case maybe_item_name of
        Nothing ->
          if null pc_item_list
             then let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.NoItemInvestory in
                  (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                   new_io : io_list, event_list, server, swdb)          
             else let new_pc = case PC.setState (PC.SelectList PC.SLPut) pc of
                        Nothing -> pc
                        Just x -> x in
                  let new_pcset = Map.insert (PC.getPhirc pc) new_pc pcset in
                  let new_io_itemlist = NS.sendMessageTo server cid $
                                        DM.makeDmMessage $ DM.List (map IT.getName pc_item_list) in
                  let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.PutSelect in
                  (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
                   new_io : new_io_itemlist : io_list, event_list, server, swdb)
        Just item_name_or_ord ->
          case reads item_name_or_ord :: [(Int, String)] of
            [(item_ord_plus1, "")] ->
              let item_ord = item_ord_plus1 - 1 in
              putItemWithOrd
              $ if length pc_item_list <= item_ord || item_ord < 0 then Nothing else Just item_ord
            _ ->
              putItemWithOrd $ findIndex (\item -> IT.getName item == item_name_or_ord) pc_item_list
            where putItemWithOrd maybe_ord =
                    case maybe_ord of
                      Nothing ->
                        let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.PutBad in
                        (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                         new_io : io_list, event_list, server, swdb)
                      Just item_ord ->
                        let pos = CH.getPosition pc in
                        let (new_pc, item) = fromJust $ CH.deleteItem item_ord pc in
                        case PM.addItem pos item phimap of
                          Nothing ->
                            let new_io =NS.sendMessageTo server cid $ DM.makeDmMessage DM.PutBadHere in
                            (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                             new_io : io_list, event_list, server, swdb)
                          Just new_phimap ->
                            let new_pcset = Map.insert (PC.getPhirc pc) new_pc pcset in
                            let new_io = NS.sendMessageTo server cid $
                                         DM.makeDmMessage $ DM.Put (CH.getName pc) (IT.getName item) in
                            (PhiWorld (new_phimap, cidset, new_pcset, npcset), pcdb,
                             new_io : io_list, event_list, server, swdb)                      
    Npc _ ->
      undefined

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (NewPc cid phirc) =
  case Map.lookup phirc pcset of
    Just _ ->
      let new_io_list = [NS.sendMessageTo server cid $ PE.encodeProtocol PE.X,
                         NS.sendMessageTo server cid $ DM.makeDmMessage DM.ChangeClientFail,
                         NS.sendMessageTo server cid $ DM.makeDmMessage DM.AccessAlready] in
      let new_event = ForceDisconnect cid in
      (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
       new_io_list ++ io_list, new_event : event_list, server, swdb)
    Nothing -> case PCD.loadPc pcdb phirc of
      Nothing ->
        let new_io_list = [NS.sendMessageTo server cid $ PE.encodeProtocol PE.X,
                           NS.sendMessageTo server cid $ DM.makeDmMessage DM.NoCharacter] in
        let new_event = ForceDisconnect cid in
        (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
         new_io_list ++ io_list, new_event : event_list, server, swdb)                     
      Just pc ->
        let new_cidset = addToAL cidset cid phirc in
        let new_pcset = Map.insert phirc pc pcset in
        let pos = CH.getPosition pc in
        let new_io_list = 
              reverse $
              sendLookMessagesToCanSeePosPc server phimap pos new_cidset new_pcset
              (Map.elems new_pcset) (Map.elems (snd npcset)) in
        let new_event_list = EV.getTriggeredEvent swdb (EV.PcPositionChange phimap pc) in
        (PhiWorld (phimap, new_cidset, new_pcset, npcset), pcdb,
         new_io_list ++ io_list, new_event_list ++ event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (PcStatusChange sctype phirc pc_change) =
  case Map.lookup phirc pcset of
    Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb)
    Just pc ->
      case pc_change pc of
        Nothing ->              
          case reverseLookUp phirc cidset of
            Nothing -> error "Assertion error (_resolveActionResult): pc doesn't have client."
            Just cid ->
              case sctype of
                PSCDirection ->
                  let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.TurnNo in
                  (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                   new_io:io_list, event_list, server, swdb)
                PSCPosition ->
                  let new_io = NS.sendMessageTo server cid $ DM.makeDmMessage DM.GoNo in
                  (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                   new_io:io_list, event_list, server, swdb)
                PSCState ->
                  (PhiWorld (phimap, cidset, pcset, npcset), pcdb,io_list,event_list, server, swdb)
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
               new_io_list ++ io_list, event_list, server, swdb)
            PSCPosition ->
              let pos = CH.getPosition new_pc in
              let new_io_list =
                    reverse $
                    sendLookMessagesToCanSeePosPc server phimap pos cidset new_pcset
                    (Map.elems new_pcset) (Map.elems (snd npcset)) in
              let new_event_list = EV.getTriggeredEvent swdb (EV.PcPositionChange phimap new_pc) in
              (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
               new_io_list ++ io_list, new_event_list ++ event_list, server, swdb)
            PSCState ->                  
              (PhiWorld (phimap, cidset, new_pcset, npcset), pcdb,
               io_list, event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (NpcStatusChange sctype nid npc_change) =
  case Map.lookup nid (snd npcset) of
    Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb)
    Just npc ->
      case npc_change npc of
        Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb,
                    io_list, event_list, server, swdb)
        Just new_npc ->
          let new_npcset = (fst npcset, Map.insert nid new_npc (snd npcset)) in
          case sctype of
            NSCDirection ->
              let pos = CH.getPosition new_npc in
              let new_io_list =
                    reverse $
                    sendLookMessagesToCanSeePosPc server phimap pos cidset pcset
                    (Map.elems pcset) (Map.elems (snd new_npcset)) in
              (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb,
               new_io_list ++ io_list, event_list, server, swdb)
            NSCPosition ->
              let pos = CH.getPosition new_npc in
              let new_io_list =
                    reverse $
                    sendLookMessagesToCanSeePosPc server phimap pos cidset pcset
                    (Map.elems pcset) (Map.elems (snd new_npcset)) in
              let new_event_list =
                    EV.getTriggeredEvent swdb (EV.NpcPositionChange phimap new_npc) in
              (PhiWorld (phimap, cidset, pcset, new_npcset), pcdb,
               new_io_list ++ io_list, new_event_list ++ event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (PcHit pc) =
  case reverseLookUp (PC.getPhirc pc) cidset of
    Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb)
    Just cid ->
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
                           map (\dm_type-> NS.sendMessageTo server tcid $ DM.makeDmMessage dm_type)
                           (CO.makeDmMessageTypeList cresult))
            $ final_target_pc_combat_list in
      (PhiWorld (phimap, cidset, new_pcset, new_npcset), pcdb,
       new_io_list_pc ++ new_io_list_target ++ io_list, event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (MessageFromDm cid msg) =
  -- ignore disconnected client
  let io = NS.sendMessageTo server cid msg in
  (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (MessageFromPc opc dpc msg) =
  case reverseLookUp (PC.getPhirc dpc) cidset of
    Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb)
    Just dcid ->
      let io = NS.sendMessageTo server dcid $
               DM.makeDmMessage (DM.PcMessage (CH.getName opc) msg)
      in (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (LogoutPc cid) =
  case lookup cid cidset of
    -- already disconnected
    Nothing -> (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb)
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
           io : new_io_list ++ io_list, event_list, server, swdb)

_resolveActionResult (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io_list, event_list, server, swdb) (ForceDisconnect cid) =
  let io = NS.disconnectClient server cid in
  (PhiWorld (phimap, cidset, pcset, npcset), pcdb, io:io_list, event_list, server, swdb)


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
  NS.SimpleTCPServer -> PhiWorld -> PCD.PlayerCharacterDB -> EV.SwitchDB ->
  (PhiWorld, PCD.PlayerCharacterDB, [IO Bool], [EV.TriggeredEvent])
charaDeadCheck server (PhiWorld (phimap, cidset, pcset, npcset)) pcdb _ =
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
