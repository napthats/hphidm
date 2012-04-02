module RunPhidm
       (
         runPhidm
       ) where

import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.State

import Msg
import PhiWorld
import ProtocolParser
import PlayerCharacterManager


runPhidm :: Chan Msg -> IO ()
runPhidm chan = do
  handle (\(SomeException _) -> return ()) (loop chan newPhiWorld)

loop :: Chan Msg -> PhiWorld -> IO ()
loop chan world = do
  msg <- readChan chan
  let (mf, mt, _) = msg
  when (mt /= getServerId || mf == getServerId) $ loop chan world
  let (resultMsgList, newWorld) = runState (execMessage msg) world
  mapM_ (writeChan chan) resultMsgList
  loop chan newWorld

execMessage :: Msg -> State PhiWorld [Msg]
execMessage msg = do
  let (mf, _, msgString) = msg 
      protocol = parseProtocolString msgString
  world <- get
  let (newMsgString, newWorld) =
        case protocol of
          Go dir -> moveCharacter 
                   

{-  
  let pos = getPlayerPosition mf world
  let cidnum = getIdNum mf
  let newPos = case msgString of
        "h" -> (\(x, y) -> (x-1, y)) pos
        "j" -> (\(x, y) -> (x, y+1)) pos
        "k" -> (\(x, y) -> (x, y-1)) pos
        "l" -> (\(x, y) -> (x+1, y)) pos
        _ -> pos
  let canMove = canMovePosition newPos world
  let (phiMap, newWorld) = setPlayerPosition mf pos newPos world
  let mapMsg = (getServerId, getBroadcastId, phiMap)
  case msgString of 
    moveMsg | moveMsg == "h" || moveMsg == "j" || moveMsg == "k" || moveMsg == "l" -> 
      if canMove
      then do put newWorld
              return [mapMsg]
      else return [mapMsg, (getServerId, mf, "You cannot go.")]
    normalMsg -> return [mapMsg, (getServerId, getBroadcastId, (show cidnum) ++ ": " ++ msgString)]
  
-}