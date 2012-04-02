import Network.Socket
import System.IO
import Control.Concurrent

import ClientId
import RunClient
import RunPhidm


main :: IO ()
main = do
  chan <- newChan
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 20017 iNADDR_ANY)
  listen sock 2
  let cid = getNewId
  _ <- forkIO $ runPhidm chan
  acceptLoop sock chan cid
 
acceptLoop :: Socket -> Chan Msg -> ClientId -> IO ()
acceptLoop sock chan cid = do
  conn <- accept sock
  _ <- forkIO $ runClient conn chan cid
  acceptLoop sock chan (getNextId cid)
 
