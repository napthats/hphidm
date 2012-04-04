-- don't call function with same SimpleTCPServer on multi-thread
module Network.SimpleTCPServer
    (
     SimpleTCPServer(),
     ClientID(),
     runTCPServer,
     getClientMessage,
     getClientMessageFrom,
     getEachClientMessage,
     broadcastMessage,
--     sendMessageTo,
    ) where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.Fix (fix)
import Data.IORef
import Data.List
import Control.Monad
import Control.Monad.STM

    
data SimpleTCPServer = SimpleTCPServer (IORef [(ClientID, TChan String)])
newtype ClientID = ClientID Integer
                   deriving (Eq, Show)


runTCPServer :: PortNumber -> IO SimpleTCPServer
runTCPServer port = do
  chanListRef <- newIORef []
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  listen sock 2
  _ <- forkIO $ acceptLoop sock chanListRef $ newClientID
  return $ SimpleTCPServer chanListRef
 
acceptLoop :: Socket -> IORef [(ClientID, TChan String)] -> ClientID -> IO ()
acceptLoop sock chanListRef cid = do
  conn <- accept sock
  chan <- atomically newTChan  
  atomicModifyIORef chanListRef (\x -> ((cid, chan):x, ()))
  _ <- forkIO $ runClient conn chan
  acceptLoop sock chanListRef $ nextClientID cid

runClient :: (Socket, SockAddr) -> TChan String -> IO ()
runClient (sock, _) chan = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
--  reader <- forkIO $ fix $ \loop -> do
--    msg <- readChan chan
--    hPutStrLn hdl msg
--    loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine hdl
    atomically $ writeTChan chan line
    loop
--  killThread reader
  hClose hdl


getClientMessage :: SimpleTCPServer -> IO (Maybe (ClientID, String))
getClientMessage (SimpleTCPServer chanListRef) = do
  chanList <- atomicModifyIORef chanListRef (\x -> (x, x))
  maybeChan <- findWithIOBool (atomically . liftM not . isEmptyTChan . snd) chanList
  case maybeChan of
    Just (cid, chan) -> do 
      msg <- atomically $ readTChan chan
      return $ Just (cid, msg)
    Nothing -> return Nothing

getClientMessageFrom :: SimpleTCPServer -> ClientID -> IO (Maybe String)
getClientMessageFrom (SimpleTCPServer chanListRef) cid = do
  chanList <- atomicModifyIORef chanListRef (\x -> (x, x))
  maybeChan <- return $ find ((==) cid . fst) chanList
  case maybeChan of
    Just (_, chan) -> do
      isEmpty <- atomically $ isEmptyTChan chan
      if isEmpty then return Nothing
                 else do
                      msg <- atomically $ readTChan chan
                      return $ Just msg
    Nothing -> error "Assertion error: illigal ClientID to getClientMessageFrom"

getEachClientMessage :: SimpleTCPServer -> IO [(ClientID, String)]
getEachClientMessage (SimpleTCPServer chanListRef) = do
  chanList <- atomicModifyIORef chanListRef (\x -> (x, x))
  notEmptyChanList <- filterWithIOBool (atomically . liftM not . isEmptyTChan . snd) chanList
  mapM (\(cid, chan) -> do {msg <- atomically $ readTChan chan; return (cid, msg)}) notEmptyChanList  

findWithIOBool :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findWithIOBool _ [] = return Nothing
findWithIOBool predict (x:xs) = do
  predResult <- predict x
  if predResult then return $ Just x
                else findWithIOBool predict xs

filterWithIOBool :: (a -> IO Bool) -> [a] -> IO [a]
filterWithIOBool _ [] = return []
filterWithIOBool predict (x:xs) = do
  predResult <- predict x
  remainder <- filterWithIOBool predict xs
  if predResult then return $ x : remainder
                else filterWithIOBool predict xs


broadcastMessage :: SimpleTCPServer -> IO ()
broadcastMessage = undefined


newClientID :: ClientID
newClientID = ClientID 0

nextClientID :: ClientID -> ClientID
nextClientID (ClientID x) = ClientID $ x + 1
