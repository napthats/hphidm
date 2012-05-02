module ClientManager
       (
         ClientIDSet(),
         PcSet(),
         PhiWorld(),
         resolveClientMessages,
         -- for debug
         makePhiWorld
       ) where

import Data.List (find)
import Data.List.Utils (addToAL, delFromAL)
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC hiding (makePlayerChara)
import qualified PlayerCharacterDB as PCD
import qualified Chara as CH
import qualified PhiMap as PM
import qualified ProtocolDecoder as PD
import qualified ProtocolEncoder as PE
import qualified DmMessages as DM


type PhiWorld = (PM.PhiMap, ClientIDSet, PcSet)
type Phirc = String

-- for debug
makePhiWorld :: PhiWorld
makePhiWorld = (PM.makePhiMap, ClientIDSet [], PcSet [])

newtype ClientIDSet = ClientIDSet [(NS.ClientID, Phirc)] deriving (Show)
newtype PcSet = PcSet [(Phirc, PC.PlayerCharacter)] deriving (Show)
reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookUp value al = case find ((== value) . snd) al of
  Nothing -> Nothing
  Just (a, _) -> Just a


resolveClientMessages ::
  NS.SimpleTCPServer -> PhiWorld -> PCD.PlayerCharacterDB -> IO (PhiWorld, PCD.PlayerCharacterDB)
resolveClientMessages server phiworld pcdb = do
  msg_list <- NS.getEachClientMessages server
  let protocol_list = map (\(cid, msg) -> (cid, PD.decodeClientMessage msg)) msg_list
  let result_list = concat $ map (\(cid, p) -> executeClientProtocol phiworld pcdb cid p) protocol_list
  resolveClientProtocolResult server result_list phiworld pcdb

resolveClientProtocolResult ::
  NS.SimpleTCPServer -> [ClientProtocolResult] -> PhiWorld -> PCD.PlayerCharacterDB ->
  IO (PhiWorld, PCD.PlayerCharacterDB)
resolveClientProtocolResult server result_list phiworld first_pcdb = do
  let (final_world, final_pcdb, final_io_list) =
        foldl (\((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io_list) result ->
                case result of
                  NewPc cid phirc pc ->
                    let new_cidset = addToAL cidset cid phirc in
                    let new_pcset = addToAL pcset phirc pc in
                    ((phimap, ClientIDSet new_cidset, PcSet new_pcset), pcdb, io_list)
                  PcStatusChange phirc pc ->
                    let new_pcset = addToAL pcset phirc pc in
                    ((phimap, ClientIDSet cidset, PcSet new_pcset), pcdb, io_list)
                  MessageFromDm cid msg ->
                    -- ignore disconnected client
                    let io = NS.sendMessageTo server cid msg in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
                  MessageFromPc opc dpc msg ->
                    case reverseLookUp (PC.getPhirc dpc) cidset of
                      Nothing -> error "Assertion error: pc doesn't have client."
                      Just dcid ->
                        let io = NS.sendMessageTo server dcid $
                                 DM.makeDmMessage (DM.PcMessage (CH.getName opc) msg)
                        in ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
                  LogoutPc cid ->
                    case lookup cid cidset of
                      -- already disconnected
                      Nothing -> ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io_list)
                      Just phirc ->
                        let io = NS.disconnectClient server cid in
                        let maybe_pc = lookup phirc pcset in
                        case maybe_pc of
                          Nothing -> error "Assertion error: client don't have pc."
                          Just pc -> 
                            let new_pcdb = PCD.savePc pcdb phirc pc in
                            let new_cidset = delFromAL cidset cid in
                            let new_pcset = delFromAL pcset phirc in
                            ((phimap, ClientIDSet new_cidset, PcSet new_pcset), 
                             new_pcdb, io:io_list)
                  ForceDisconnect cid ->
                    let io = NS.disconnectClient server cid in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
          ) (phiworld, first_pcdb, []) result_list
  mapM_ (\x -> do _ <- x; return ()) $ reverse final_io_list
  return (final_world, final_pcdb)

data ClientProtocolResult = NewPc NS.ClientID Phirc PC.PlayerCharacter
                          | PcStatusChange Phirc PC.PlayerCharacter
                          | MessageFromDm NS.ClientID String
                          | MessageFromPc PC.PlayerCharacter PC.PlayerCharacter String
--                          | NormalMessage PC.PlayerChara String
--                          | BroadcastMessage PC.PlayerChara String
                          | LogoutPc NS.ClientID
                          | ForceDisconnect NS.ClientID
                          deriving (Show)


-- returned results have to be excuted in an order of the list
executeClientProtocol ::
  PhiWorld -> PCD.PlayerCharacterDB -> NS.ClientID -> PD.ClientProtocol -> [ClientProtocolResult]
executeClientProtocol (phimap, ClientIDSet cidset, PcSet pcset) pcdb cid protocol =
  let maybe_pc = case lookup cid cidset of 
        Nothing -> Nothing
        Just phirc -> case lookup phirc pcset of
          Nothing -> error "PlayerCharacter is unregistered in PcSet"
          Just pc -> Just pc
  in case maybe_pc of
    Nothing -> case protocol of
      PD.Open phirc -> case lookup phirc pcset of
        Just _ -> [MessageFromDm cid $ DM.makeDmMessage DM.AccessAlready,
                   MessageFromDm cid $ DM.makeDmMessage DM.ChangeClientFail,
                   MessageFromDm cid $ PE.encodeProtocol PE.X,
                   ForceDisconnect cid]
        Nothing -> case PCD.loadPc pcdb phirc of
          Nothing -> [MessageFromDm cid $ DM.makeDmMessage DM.NoCharacter,
                      MessageFromDm cid $ PE.encodeProtocol PE.X,
                      ForceDisconnect cid]
          Just new_pc -> [NewPc cid phirc new_pc] ++ makeLookResult cid phimap new_pc pcset cidset
      _ -> []
    Just pc -> case protocol of
      PD.Go dir -> let maybe_modified_pc = CH.walk phimap dir pc in
                   case maybe_modified_pc of
                     Nothing -> [MessageFromDm cid $ DM.makeDmMessage DM.GoNo]
                     Just modified_pc ->
                       let phirc = case lookup cid cidset of
                             Nothing -> error "Assertion error"
                             Just x -> x
                       in [PcStatusChange phirc modified_pc] ++
                          makeLookResult cid phimap modified_pc pcset cidset
      PD.Turn maybe_dir -> case maybe_dir of
        Just dir -> let modified_pc = CH.turn dir pc in
                    let phirc = case lookup cid cidset of
                          Nothing -> error "Assertion error"
                          Just x -> x
                    in [PcStatusChange phirc modified_pc] ++
                       makeLookResult cid phimap modified_pc pcset cidset
        Nothing -> [MessageFromDm cid $ DM.makeDmMessage DM.TurnBad]
      PD.RawMessage msg ->
        let visible_pos_list = PM.getVisiblePositions PM.All phimap
                               (CH.getPosition pc) (CH.getDirection pc) sightWidth sightHeight
        in let visible_chara_list = CH.getCharaInRegion visible_pos_list (map snd pcset) in
        map (\(_, _, vchara) -> MessageFromPc pc vchara msg) visible_chara_list
      PD.Exit -> [MessageFromDm cid $ DM.makeDmMessage DM.Savedata,
                  MessageFromDm cid $ DM.makeDmMessage DM.Seeyou,
                  MessageFromDm cid $ PE.encodeProtocol PE.Close,
                  LogoutPc cid]
      PD.Open _ -> []
      PD.UnknownProtocol -> []

makeLookResult :: 
  NS.ClientID -> PM.PhiMap -> PC.PlayerCharacter ->
  [(Phirc, PC.PlayerCharacter)] -> [(NS.ClientID, Phirc)] -> [ClientProtocolResult]
makeLookResult cid phimap pc pcset cidset =
  case lookup cid cidset of
    Nothing -> 
      let charaset = pc : map snd pcset in
      _makeLookResult cid phimap pc charaset
    Just phirc -> 
      let charaset = pc : map snd (delFromAL pcset phirc) in
      _makeLookResult cid phimap pc charaset
      
_makeLookResult ::
  (CH.Chara a) => NS.ClientID -> PM.PhiMap -> PC.PlayerCharacter -> [a] -> [ClientProtocolResult]
_makeLookResult cid phimap pc charaset =
  [MessageFromDm cid $ makeAroundView phimap pc] ++
  map (MessageFromDm cid) (makeAroundCharaView phimap pc charaset) ++
  [MessageFromDm cid $ PE.encodeProtocol PE.M57End]

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
