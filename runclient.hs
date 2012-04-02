module RunClient
       (
         Msg,
         runClient
       ) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix (fix)

import Msg


runClient :: (Socket, SockAddr) -> Chan Msg -> ClientId -> IO ()
runClient (sock, _) chan nr = do
  let sendMessage mt msg = writeChan chan (nr, mt, msg)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  chan' <- dupChan chan
  reader <- forkIO $ fix $ \loop -> do
    (_, mt, line) <- readChan chan'
    when (nr == mt) $ hPutStrLn hdl line
    loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine hdl
    case line of
      "quit" -> hPutStrLn hdl "Bye!"
      _      -> do
        sendMessage getServerId (line)
        loop
  killThread reader
  hClose hdl

