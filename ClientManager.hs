module ClientManager
       (
         ClientIDSet(),
         PcSet(),
         PhiWorld(),
         resolveClientMessages,
         -- for debug
         makePhiWorld
       ) where

--import Data.List (find)
import Data.List.Utils (addToAL, delFromAL)
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC hiding (makePlayerChara)
import qualified PlayerCharacterDB as PCD
import qualified Chara as CH
import qualified PhiMap as PM
import qualified ProtocolDecoder as PD
import qualified ProtocolEncoder as PE


type PhiWorld = (PM.PhiMap, ClientIDSet, PcSet)
type Phirc = String

-- for debug
makePhiWorld :: PhiWorld
makePhiWorld = (PM.makePhiMap, ClientIDSet [], PcSet [])

newtype ClientIDSet = ClientIDSet [(NS.ClientID, Phirc)] deriving (Show)
newtype PcSet = PcSet [(Phirc, PC.PlayerCharacter)] deriving (Show)
--reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
--reverseLookUp value al = case find ((== value) . snd) al of
--  Nothing -> Nothing
--  Just (a, _) -> Just a


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
                  PrivateMessage cid msg ->
                    -- ignore disconnected client
                    let io = NS.sendMessageTo server cid msg in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
                  LogoutPc cid ->
                    case lookup cid cidset of
                      -- already disconnected
                      Nothing -> ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io_list)
                      Just phirc ->
                        let io1 = NS.sendMessageTo server cid $ PE.encodeProtocol PE.Close in
                        let io2 = NS.disconnectClient server cid in
                        let maybe_pc = lookup phirc pcset in
                        case maybe_pc of
                          Nothing -> error "Assertion error"
                          Just pc -> 
                            let new_pcdb = PCD.savePc pcdb phirc pc in
                            let new_cidset = delFromAL cidset cid in
                            let new_pcset = delFromAL pcset phirc in
                            ((phimap, ClientIDSet new_cidset, PcSet new_pcset), 
                             new_pcdb, io2:io1:io_list)
                  ForceDisconnectByNoPc cid ->
                    let io1 = NS.sendMessageTo server cid $ PE.encodeProtocol PE.X in
                    let io2 = NS.disconnectClient server cid in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io2:io1:io_list)
                  ForceDisconnectByDupPc cid ->
                    let io1 = NS.sendMessageTo server cid $ PE.encodeProtocol PE.X in
                    let io2 = NS.disconnectClient server cid in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io2:io1:io_list)
          ) (phiworld, first_pcdb, []) result_list
  mapM_ (\x -> do _ <- x; return ()) $ reverse final_io_list
  return (final_world, final_pcdb)

data ClientProtocolResult = NewPc NS.ClientID Phirc PC.PlayerCharacter
                          | PcStatusChange Phirc PC.PlayerCharacter
                          | PrivateMessage NS.ClientID String
--                          | NormalMessage PC.PlayerChara String
--                          | BroadcastMessage PC.PlayerChara String
                          | LogoutPc NS.ClientID
                          | ForceDisconnectByNoPc NS.ClientID
                          | ForceDisconnectByDupPc NS.ClientID
                          deriving (Show)

-- returned results have to be excuted in an order of the list
executeClientProtocol ::
  PhiWorld -> PCD.PlayerCharacterDB -> NS.ClientID -> PD.ClientProtocol -> [ClientProtocolResult]
executeClientProtocol (phi_map, ClientIDSet cidset, PcSet pcset) pcdb cid protocol =
  let maybe_pc = case lookup cid cidset of 
        Nothing -> Nothing
        Just phirc -> case lookup phirc pcset of
          Nothing -> error "PlayerCharacter is unregistered in PcSet"
          Just pc -> Just pc
  in case maybe_pc of
    Nothing -> case protocol of
      PD.Open phirc -> case lookup phirc pcset of
        Just _ -> [ForceDisconnectByDupPc cid]
        Nothing -> case PCD.loadPc pcdb phirc of
          Nothing -> [ForceDisconnectByNoPc cid]
          Just new_pc -> [NewPc cid phirc new_pc] ++ makeLookResult cid phi_map new_pc
      _ -> []
    Just pc -> case protocol of
      PD.Go maybe_dir -> case maybe_dir of
        Just dir -> let maybe_modified_pc = CH.walk phi_map dir pc
                    in case maybe_modified_pc of
                      Nothing -> []
                      Just modified_pc ->
                        let phirc = case lookup cid cidset of
                                 Nothing -> error "Assertion error"
                                 Just x -> x
                        in [PcStatusChange phirc modified_pc] ++ makeLookResult cid phi_map modified_pc
        Nothing -> []
      PD.Exit -> [LogoutPc cid]
      _ -> []

makeLookResult :: NS.ClientID -> PM.PhiMap -> PC.PlayerCharacter -> [ClientProtocolResult]
makeLookResult cid phimap pc =
  [PrivateMessage cid $ makeAroundView phimap pc,
   PrivateMessage cid $ PE.encodeProtocol PE.M57End]

makeAroundView :: PM.PhiMap -> PC.PlayerCharacter -> String
makeAroundView phi_map pc =
  PE.encodeProtocol $ PE.M57Map (CH.getDirection pc) $
  PM.getMapView PM.All phi_map (CH.getPosition pc) (CH.getDirection pc) 7 7
