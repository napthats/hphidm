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
     disconnectClient,
     shutdownServer,
    ) where

--import Network.Socket
import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.Fix (fix)
import Data.IORef
import Data.List
import Data.String.Utils (replace)
import Control.Monad
import Control.Monad.STM
import qualified Utils.Id as UI

    
newtype SimpleTCPServer = SimpleTCPServer (IORef [Client], Socket, ThreadId)
newtype ClientID = ClientID UI.Id
                 deriving (Eq, Show)
data ClientStatus = Live | Dead deriving (Show)
type Client = (ClientID, TChan String, TChan String, IORef ClientStatus, ThreadId)

newClientID :: ClientID
newClientID = ClientID UI.newId

nextClientID :: ClientID -> ClientID
nextClientID (ClientID x) = ClientID $ UI.nextId x

getClientID :: Client -> ClientID
getClientID (cid, _, _, _, _) = cid 

getWchan :: Client -> TChan String
getWchan (_, wchan, _, _, _) = wchan

isLive :: Client -> IO Bool
isLive (_, _, _, stref, _) = do
  st <- atomicModifyIORef stref (\x -> (x, x))
  case st of 
    Dead -> return False
    Live -> return True

shutdownServer :: SimpleTCPServer -> IO ()
shutdownServer (SimpleTCPServer (_, sock, threadid)) = do
  killThread threadid
  sClose sock

disconnectClient :: SimpleTCPServer -> ClientID -> IO Bool
disconnectClient (SimpleTCPServer (clientListRef, _, _)) cid = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeClient <- return $ find ((==) cid . getClientID) clientList
  case maybeClient of 
    Just (_, _, _, _, cthread) -> do {killThread cthread; return True}
    Nothing -> return False

runTCPServer :: PortNumber -> IO SimpleTCPServer
runTCPServer port = do
  clientListRef <- newIORef []
  sock <- listenOn $ PortNumber port
  threadid <- forkIO $ acceptLoop sock clientListRef newClientID
  _ <- forkIO $ clientStatusCheckLoop clientListRef
  return $ SimpleTCPServer (clientListRef, sock, threadid)

clientStatusCheckLoop :: IORef [Client] -> IO ()
clientStatusCheckLoop clientListRef = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  toDeleteClientList <- filterM (liftM not . isLive) clientList
  mapM_ (\(_, _, _, _, cthread) -> killThread cthread) toDeleteClientList
  atomicModifyIORef clientListRef
    (\list -> (filter
               (\client -> notElem (getClientID client) (map getClientID toDeleteClientList))
               list, ()))
  threadDelay $ 1000 * 1000
  clientStatusCheckLoop clientListRef
 
acceptLoop :: Socket -> IORef [Client] -> ClientID -> IO ()
acceptLoop sock clientListRef cid = do
  (hdl, _, _) <- accept sock
  wchan <- atomically newTChan
  rchan <- atomically newTChan
  stref <- newIORef Live
  cthread <- forkIO $ runClient hdl wchan rchan stref
  atomicModifyIORef clientListRef (\x -> ((cid, wchan, rchan, stref, cthread):x, ()))
  acceptLoop sock clientListRef $ nextClientID cid

runClient :: Handle -> TChan String -> TChan String -> IORef ClientStatus -> IO ()
runClient hdl wchan rchan stref = do
  hSetBuffering hdl NoBuffering
  reader <- forkIO $ fix $ \loop -> do
    msg <- atomically $ readTChan rchan
    hPutStrLn hdl msg
    loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine hdl
    let chomped_line = replace "\r" "" line
    atomically $ writeTChan wchan chomped_line
    loop
  killThread reader
  atomicModifyIORef stref (\_ -> (Dead, ()))
  hClose hdl

--return a message if exists
--it's oldest about messages of same client
--no consistency about priority of clients
getClientMessage :: SimpleTCPServer -> IO (Maybe (ClientID, String))
getClientMessage (SimpleTCPServer (clientListRef, _, _)) = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeChan <- findM (atomically . liftM not . isEmptyTChan . getWchan) clientList
  case maybeChan of
    Just (cid, wchan, _, _, _) -> do 
      msg <- atomically $ readTChan wchan
      return $ Just (cid, msg)
    Nothing -> return Nothing

--return a message from a certain client if exists
--it's oldest about messages of same client
getClientMessageFrom :: SimpleTCPServer -> ClientID -> IO (Maybe String)
getClientMessageFrom (SimpleTCPServer (clientListRef, _, _)) cid = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeClient <- return $ find ((==) cid . getClientID) clientList
  case maybeClient of
    Just (_, wchan, _, _, _) -> do
      isEmpty <- atomically $ isEmptyTChan wchan
      if isEmpty then return Nothing
                 else do
                      msg <- atomically $ readTChan wchan
                      return $ Just msg
    Nothing -> return Nothing

--return a list of one message for each client.
--ignore client with no message (so length of the list is the number of clients with messages)
--it's oldest about messages of same client
--no consistency about an order of messages
getEachClientMessages :: SimpleTCPServer -> IO [(ClientID, String)]
getEachClientMessages (SimpleTCPServer (clientListRef, _, _)) = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  notEmptyChanList <- filterM (atomically . liftM not . isEmptyTChan . getWchan) clientList
  mapM (\(cid, wchan, _, _, _) -> do {msg <- atomically $ readTChan wchan; return (cid, msg)}) notEmptyChanList  

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM predict (x:xs) = do
  predResult <- predict x
  if predResult then return $ Just x
                else findM predict xs


broadcastMessage :: SimpleTCPServer -> String -> IO ()
broadcastMessage (SimpleTCPServer (clientListRef, _, _)) msg = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  mapM_ (\(_, _, rchan, _, _) -> atomically $ writeTChan rchan msg) clientList

--return if a message can be sent
--False means that a client already disconnected
--disconnect check is executed every seconds
sendMessageTo :: SimpleTCPServer -> ClientID -> String -> IO Bool
sendMessageTo (SimpleTCPServer (clientListRef, _, _)) cid msg = do
  clientList <- atomicModifyIORef clientListRef (\x -> (x, x))
  maybeClient <- return $ find ((==) cid . getClientID) clientList
  case maybeClient of
    Just (_, _, rchan, _, _) -> do
      atomically $ writeTChan rchan msg
      return True
    Nothing -> return False

