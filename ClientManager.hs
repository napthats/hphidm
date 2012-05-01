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
import Control.Monad (liftM)
import qualified Network.SimpleTCPServer as NS
import qualified Chara as PC
import qualified PhiMap as PM
import qualified ProtocolDecoder as PD
import qualified ProtocolEncoder as PE


type PhiWorld = (PM.PhiMap, ClientIDSet, PcSet)
type Phirc = String

-- for debug
makePhiWorld :: PhiWorld
makePhiWorld = (PM.makePhiMap, ClientIDSet [], PcSet [])

newtype ClientIDSet = ClientIDSet [(NS.ClientID, Phirc)] deriving (Show)
newtype PcSet = PcSet [(Phirc, PC.PlayerChara)] deriving (Show)
--reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
--reverseLookUp value al = case find ((== value) . snd) al of
--  Nothing -> Nothing
--  Just (a, _) -> Just a


resolveClientMessages :: NS.SimpleTCPServer -> PhiWorld -> IO PhiWorld
resolveClientMessages server phiworld = do
  msg_list <- NS.getEachClientMessages server
  let protocol_list = map (\(cid, msg) -> (cid, PD.decodeClientMessage msg)) msg_list
  result_list <- liftM concat $ mapM (\(cid, p) -> executeClientProtocol phiworld cid p) protocol_list
  resolveClientProtocolResult server result_list phiworld

resolveClientProtocolResult :: NS.SimpleTCPServer -> [ClientProtocolResult] -> PhiWorld -> IO PhiWorld
resolveClientProtocolResult server result_list phiworld =
  foldl (\current_world result -> case result of
            NewPc cid phirc pc -> do
              (phimap, ClientIDSet cidset, PcSet pcset) <- current_world
              let new_cidset = addToAL cidset cid phirc
              let new_pcset = addToAL pcset phirc pc
              return (phimap, ClientIDSet new_cidset, PcSet new_pcset)
            PcStatusChange phirc pc -> do
              (phimap, ClientIDSet cidset, PcSet pcset) <- current_world
              let new_pcset = addToAL pcset phirc pc
              return (phimap, ClientIDSet cidset, PcSet new_pcset)
            PrivateMessage cid msg -> do
              -- ignore disconnected client
              _ <- NS.sendMessageTo server cid msg
              current_world
            LogoutPc cid -> do
              (phimap, ClientIDSet cidset, PcSet pcset) <- current_world
              case lookup cid cidset of
                -- already disconnected
                Nothing -> current_world
                Just phirc -> do
                  _ <- NS.sendMessageTo server cid $ PE.encodeProtocol PE.Close
                  _ <- NS.disconnectClient server cid
                  let new_cidset = delFromAL cidset cid
                  let new_pcset = delFromAL pcset phirc
                  return (phimap, ClientIDSet new_cidset, PcSet new_pcset)
          ) (return phiworld) result_list

data ClientProtocolResult = NewPc NS.ClientID Phirc PC.PlayerChara
                          | PcStatusChange Phirc PC.PlayerChara
                          | PrivateMessage NS.ClientID String
--                          | NormalMessage PC.PlayerChara String
--                          | BroadcastMessage PC.PlayerChara String
                          | LogoutPc NS.ClientID
                          deriving (Show)

-- returned results have to be excuted in an order of the list
executeClientProtocol :: PhiWorld -> NS.ClientID -> PD.ClientProtocol -> IO [ClientProtocolResult]
executeClientProtocol (phi_map, ClientIDSet cidset, PcSet pcset) cid protocol =
  let maybe_pc = case lookup cid cidset of 
        Nothing -> Nothing --error "ClientID is unregistered in ClientIDSet"
        Just phirc -> case lookup phirc pcset of
          Nothing -> error "PlayerCharacter is unregistered in PcSet"
          Just pc -> Just pc
  in case maybe_pc of
    Nothing -> case protocol of
      PD.Open phirc -> do let new_pc = PC.makePlayerChara phi_map phirc phirc
                          view <- makeAroundView phi_map new_pc
                          return $ [NewPc cid phirc new_pc, PrivateMessage cid view]
      _ -> return []
    Just pc -> case protocol of
      PD.Go maybe_dir -> case maybe_dir of
        Just dir -> do let maybe_modified_pc = PC.walk phi_map dir pc
                       case maybe_modified_pc of
                         Nothing -> return []
                         Just modified_pc -> do
                           view <- makeAroundView phi_map modified_pc
                           let phirc = case lookup cid cidset of
                                 Nothing -> error "Assertion error"
                                 Just x -> x
                           return [PcStatusChange phirc modified_pc, PrivateMessage cid view]
        Nothing -> return []
      PD.Exit -> return [LogoutPc cid]
      _ -> return []

makeAroundView :: PM.PhiMap -> PC.PlayerChara -> IO String
makeAroundView phi_map pc =
  return $ PE.encodeProtocol $ PE.M57Map (PC.getDirection pc) $
    PM.getMapView PM.All phi_map (PC.getPosition pc) (PC.getDirection pc) 7 7
