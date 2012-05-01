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
resolveClientProtocolResult server result_list phiworld pcdb =
  foldl (\current_both result -> do
            ((phimap, ClientIDSet cidset, PcSet pcset), current_pcdb) <- current_both
            case result of
              NewPc cid phirc pc -> do
                let new_cidset = addToAL cidset cid phirc
                let new_pcset = addToAL pcset phirc pc
                return ((phimap, ClientIDSet new_cidset, PcSet new_pcset), current_pcdb)
              PcStatusChange phirc pc -> do
                let new_pcset = addToAL pcset phirc pc
                return ((phimap, ClientIDSet cidset, PcSet new_pcset), current_pcdb)
              PrivateMessage cid msg -> do
                -- ignore disconnected client
                _ <- NS.sendMessageTo server cid msg
                current_both
              LogoutPc cid -> do
                case lookup cid cidset of
                  -- already disconnected
                  Nothing -> current_both
                  Just phirc -> do
                    _ <- NS.sendMessageTo server cid $ PE.encodeProtocol PE.Close
                    _ <- NS.disconnectClient server cid
                    let maybe_pc = lookup phirc pcset
                    case maybe_pc of
                      Nothing -> error "Assertion error"
                      Just pc -> do let new_pcdb = PCD.savePc pcdb phirc pc
                                    let new_cidset = delFromAL cidset cid
                                    let new_pcset = delFromAL pcset phirc
                                    return((phimap, ClientIDSet new_cidset, PcSet new_pcset), new_pcdb)
              ForceDisconnectByNoPc cid -> do
                _ <- NS.sendMessageTo server cid $ PE.encodeProtocol PE.X
                _ <- NS.disconnectClient server cid
                current_both
              ForceDisconnectByDupPc cid -> do
                _ <- NS.sendMessageTo server cid $ PE.encodeProtocol PE.X
                _ <- NS.disconnectClient server cid
                current_both
          ) (return (phiworld, pcdb)) result_list

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
          Just new_pc -> let view = makeAroundView phi_map new_pc
                         in [NewPc cid phirc new_pc, PrivateMessage cid view]
      _ -> []
    Just pc -> case protocol of
      PD.Go maybe_dir -> case maybe_dir of
        Just dir -> let maybe_modified_pc = CH.walk phi_map dir pc
                    in case maybe_modified_pc of
                      Nothing -> []
                      Just modified_pc ->
                        let view = makeAroundView phi_map modified_pc
                        in let phirc = case lookup cid cidset of
                                 Nothing -> error "Assertion error"
                                 Just x -> x
                        in [PcStatusChange phirc modified_pc, PrivateMessage cid view]
        Nothing -> []
      PD.Exit -> [LogoutPc cid]
      _ -> []

makeAroundView :: PM.PhiMap -> PC.PlayerCharacter -> String
makeAroundView phi_map pc =
  PE.encodeProtocol $ PE.M57Map (CH.getDirection pc) $
  PM.getMapView PM.All phi_map (CH.getPosition pc) (CH.getDirection pc) 7 7
