module ClientManager
       (
         ClientIDSet(),
         executeClientMessages,
       ) where

import Data.List (find)
import qualified Network.SimpleTCPServer as NS
import qualified Chara as PC
import qualified PhiMap as PM
import qualified ProtocolDecoder as PD
import qualified Utils.MyList as ML
import qualified ProtocolEncoder as PE


newtype ClientIDSet = ClientIDSet [(NS.ClientID, String)] deriving (Show)

executeClientMessages :: NS.SimpleTCPServer -> (PM.PhiMap, ClientIDSet, [PC.PlayerChara]) ->
                         IO (PM.PhiMap, ClientIDSet, [PC.PlayerChara])
executeClientMessages server (phi_map, ClientIDSet id_set, chara_list) = do
  msg_list <- NS.getEachClientMessages server
  --
  mapM_ (\(_, msg) -> putStrLn msg) msg_list
  --
  let protocol_list_phirc =
        map (\(cid, msg) -> (cid, PD.decodeClientMessage msg, (lookup cid id_set))) msg_list
  let protocol_list_pc = map (\(cid, msg, maybe_phirc) -> case maybe_phirc of
                            Nothing -> (cid, msg, Nothing)
                            Just phirc -> case find (\pc -> case PC.getPhirc pc of
                                                        Nothing -> False
                                                        Just p -> phirc == p)
                                               chara_list of
                                            Nothing -> (cid, msg, Nothing)
                                            Just pc -> (cid, msg, Just pc))
                    protocol_list_phirc
  maybe_result_pc_list <- mapM (executeClientProtocol server phi_map) protocol_list_pc
  let result_pc_list = ML.filterMaybe maybe_result_pc_list    
  let additional_id_set = map (\(new_cid, new_pc) -> (new_cid, case PC.getPhirc new_pc of
                                                         Nothing -> ""
                                                         Just phirc -> phirc
                                                     )) result_pc_list
  let new_id_set = filter (\(old_id, _) -> case lookup old_id additional_id_set of
                              Nothing -> True; Just _ -> False) id_set ++
                   additional_id_set
  let new_chara_set =
        filter (\old_pc -> case find
                                (\new_pc -> PC.getPhirc new_pc == PC.getPhirc old_pc) $
                                map snd result_pc_list of
                             Nothing -> True
                             Just _ -> False
               ) chara_list ++
        map snd result_pc_list
  return (phi_map, ClientIDSet new_id_set, new_chara_set)

executeClientProtocol :: NS.SimpleTCPServer -> 
                         PM.PhiMap -> (NS.ClientID, PD.ClientProtocol, Maybe PC.PlayerChara) ->
                         IO (Maybe (NS.ClientID, PC.PlayerChara))
executeClientProtocol server phi_map (cid, protocol, maybe_pc) =
  case maybe_pc of
    Nothing -> case protocol of
      PD.Open phirc -> do let new_pc = PC.makePlayerChara phi_map phirc phirc
                          sendAroundView server cid phi_map new_pc
                          return $ Just (cid, new_pc)
      _ -> return Nothing
    Just pc -> case protocol of
      PD.Go maybe_dir -> case maybe_dir of
        Just dir -> do let new_pc = PC.walk phi_map dir pc
                       sendAroundView server cid phi_map new_pc
                       return (Just (cid, new_pc))
        Nothing -> return Nothing
      _ -> return Nothing

sendAroundView :: NS.SimpleTCPServer -> NS.ClientID -> PM.PhiMap -> PC.PlayerChara -> IO ()
sendAroundView server cid phi_map pc = do
  _ <- NS.sendMessageTo server cid $
       PE.makeM57MapProtocol (PC.getDirection pc) $
       PM.getMapView PM.All phi_map (PC.getPosition pc) (PC.getDirection pc) 7 7
  return ()
