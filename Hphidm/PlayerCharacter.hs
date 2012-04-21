module Hphidm.PlayerCharacter
       (
         PlayerCharacter(),
         move,
        ) where

import qualified Hphidm.PhiMap as PM
import qualified Hphidm.ServerMessage as SM
import qualified Utils.Id as UI


newtype PlayerCharacter = PlayerCharacter (PlayerID, Name, PM.Position)
newtype Name = Name String
newtype PlayerID = PlayerID UI.Id


move :: PlayerCharacter -> PM.Direction -> IO (PlayerCharacter, SM.ServerMessage)
move pc dir = do
  pos <- getPosWithDir pc dir
  canmove <- canEnter pc pos
  return $ if canmove then (changePos pc pos, SM.NoMessage)
                      else (pc, SM.ToPcMessage "Can not go.")
                          
getPosWithDir :: PlayerCharacter -> PM.Direction -> IO PM.Position
getPosWithDir (PlayerCharacter (_, _, pos)) dir = PM.getPosWithDir pos dir

canEnter :: PlayerCharacter -> PM.Position -> IO Bool
canEnter _ pos = PM.canEnter pos

changePos :: PlayerCharacter -> PM.Position -> PlayerCharacter
changePos (PlayerCharacter (id, name, pos)) newpos = PlayerCharacter (id, name, newpos)
