import Control.Concurrent
import Network.SimpleTCPServer

main :: IO ()
main = do
  t <- runTCPServer 20017
--  mainloop t Nothing
  mainloop2 t
  
mainloop :: SimpleTCPServer -> Maybe ClientID -> IO ()
mainloop t prevCid = do
  nextCid <- case prevCid of
    Just pc -> do
      m <- getClientMessageFrom t pc
      case m of
        Just msg -> do
          putStrLn $ show msg
          putStrLn $ show pc
        Nothing -> putStrLn "No msg"
      return prevCid
    Nothing -> do
      m <- getClientMessage t
      case m of
        Just (cid, msg) -> do
          putStrLn $ show msg
          putStrLn $ show cid
          return $ Just cid
        Nothing -> do
                   putStrLn "No msg"
                   return prevCid
  threadDelay $ 1000 * 1000
  mainloop t nextCid

mainloop2 :: SimpleTCPServer -> IO ()
mainloop2 t = do
  m <- getEachClientMessage t
  putStrLn "----------"
  mapM_ (putStrLn . show) m
  mapM_ (\(cid, msg) -> sendMessageTo t cid $ "yours: " ++ show msg) m
  mapM_ (\(cid, msg) -> broadcastMessage t $ show cid ++ ": " ++ show msg) m
  threadDelay $ 1000 * 1000 * 2  
  mainloop2 t
