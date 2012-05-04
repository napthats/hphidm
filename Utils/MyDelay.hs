module Utils.MyDelay
       (
         delayForFramerate,
       ) where

import System.Time (ClockTime(..))
import Control.Concurrent (threadDelay)


-- delay(sleep) first argument: milliseconds - (current time - previous time)
delayForFramerate :: Int -> ClockTime -> ClockTime -> IO ()
delayForFramerate time_per_frame cur pre = do
  let time_diff = case cur of
        (TOD cur_s cur_p) -> case pre of
          (TOD pre_s pre_p) ->
            (cur_s - pre_s) * 1000000 + (cur_p - pre_p) `div` 1000000
  let wait_time = max 0 $ time_per_frame * 1000 - (fromIntegral time_diff)
  threadDelay $ wait_time



{-
-- delay(sleep) first argument: milliseconds - (current time - previous time)
delayForFramerate :: Int -> ClockTime -> ClockTime -> IO ()
delayForFramerate time_per_frame cur pre = do
  time_diff <- case cur of
        (TOD cur_s cur_p) -> case pre of
          (TOD pre_s pre_p) -> do
            putStrLn $ show cur_s ++ ":" ++ show cur_p ++ ", " ++ show pre_s ++ ":" ++ show pre_p
            return $ (cur_s - pre_s) * 1000000 + (cur_p - pre_p) `div` 1000000
  let wait_time = max 0 $ time_per_frame * 1000 - (fromIntegral time_diff)
--  let wait_time = time_per_frame * 1000 - (fromIntegral time_diff)
  putStrLn $ show time_diff
  putStrLn $ show wait_time
  threadDelay $ wait_time
-}