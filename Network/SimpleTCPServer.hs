module Network.SimpleTCPServer
    (
     SimpleTCPServer(),
     ClientID(),
     runTCPServer,
     getClientMessage,
     getClientMessageFrom,
     getEachClientMessages,
     broadcastMessage,
     sendMessageTo,
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

    
data SimpleTCPServer = SimpleTCPServer (IORef [Client])
newtype ClientID = ClientID Integer
                   deriving (Eq, Show)
data ClientStatus = Live | Dead
type Client = (ClientID, TChan String, TChan String, IORef ClientStatus)

newClientID :: ClientID
newClientID = ClientID 0

nextClientID :: ClientID -> ClientID
nextClientID (ClientID x) = ClientID $ x + 1

getClientID :: Client -> ClientID
getClientID (cid, _, _, _) = cid 

getWchan :: Client -> TChan String
getWchan (_, wchan, _, _) = wchan

--getRchan :: Client -> TChan String
--getRchan (_, _, rchan, _) = rchan

--getStref :: Client -> IORef ClientStatus
--getStref (_, _, _, stref) = stref

isLive :: Client -> IO Bool
isLive (_, _, _, stref) = do
  st <- atomicModifyIORef stref (\x -> (x, x))
  case st of 
    Dead -> return False
    Live -> return True


runTCPServer :: PortNumber -> IO SimpleTCPServer
runTCPServer port = do
  clientListRef <- newIORef []
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  listen sock 2
  _ <- forkIO $ acceptLoop sock clientListRef newClientID
  _ <- forkIO $ clientStatusCheckLoop clientListRef
  return $ SimpleTCPServer clientListRef

clientStatusCheckLoop :: IORef [Client] -> IO ()
clientStatusCheckLoop clientListRef = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  putStrLn $ show $ map getClientID clientList
  toDeleteClientList <- filterWithIOBool (liftM not . isLive) clientList
  atomicModifyIORef clientListRef
    (\list -> (filter
               (\client -> notElem (getClientID client) (map getClientID toDeleteClientList))
               list, ()))
  threadDelay $ 1000 * 1000
  clientStatusCheckLoop clientListRef
 
acceptLoop :: Socket -> IORef [Client] -> ClientID -> IO ()
acceptLoop sock clientListRef cid = do
  conn <- accept sock
  wchan <- atomically newTChan
  rchan <- atomically newTChan
  stref <- newIORef Live
  atomicModifyIORef clientListRef (\x -> ((cid, wchan, rchan, stref):x, ()))
  _ <- forkIO $ runClient conn wchan rchan stref
  acceptLoop sock clientListRef $ nextClientID cid

runClient :: (Socket, SockAddr) -> TChan String -> TChan String -> IORef ClientStatus -> IO ()
runClient (sock, _) wchan rchan stref = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  reader <- forkIO $ fix $ \loop -> do
    msg <- atomically $ readTChan rchan
    hPutStrLn hdl msg
    loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine hdl
    atomically $ writeTChan wchan line
    loop
  killThread reader
  atomicModifyIORef stref (\_ -> (Dead, ()))
  hClose hdl


getClientMessage :: SimpleTCPServer -> IO (Maybe (ClientID, String))
getClientMessage (SimpleTCPServer clientListRef) = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeChan <- findWithIOBool (atomically . liftM not . isEmptyTChan . getWchan) clientList
  case maybeChan of
    Just (cid, wchan, _, _) -> do 
      msg <- atomically $ readTChan wchan
      return $ Just (cid, msg)
    Nothing -> return Nothing

getClientMessageFrom :: SimpleTCPServer -> ClientID -> IO (Maybe String)
getClientMessageFrom (SimpleTCPServer clientListRef) cid = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeClient <- return $ find ((==) cid . getClientID) clientList
  case maybeClient of
    Just (_, wchan, _, _) -> do
      isEmpty <- atomically $ isEmptyTChan wchan
      if isEmpty then return Nothing
                 else do
                      msg <- atomically $ readTChan wchan
                      return $ Just msg
    Nothing -> return Nothing

getEachClientMessages :: SimpleTCPServer -> IO [(ClientID, String)]
getEachClientMessages (SimpleTCPServer clientListRef) = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  notEmptyChanList <- filterWithIOBool (atomically . liftM not . isEmptyTChan . getWchan) clientList
  mapM (\(cid, wchan, _, _) -> do {msg <- atomically $ readTChan wchan; return (cid, msg)}) notEmptyChanList  

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


broadcastMessage :: SimpleTCPServer -> String -> IO ()
broadcastMessage (SimpleTCPServer clientListRef) msg = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  mapM_ (\(_, _, rchan, _) -> atomically $ writeTChan rchan msg) clientList

sendMessageTo :: SimpleTCPServer -> ClientID -> String -> IO Bool
sendMessageTo (SimpleTCPServer clientListRef) cid msg = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeClient <- return $ find ((==) cid . getClientID) clientList
  case maybeClient of
    Just (_, _, rchan, _) -> do
      atomically $ writeTChan rchan msg
      return True
    Nothing -> return False
