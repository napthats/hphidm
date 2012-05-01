module PlayerCharacter
       (
         PlayerCharacter(),
         makePlayerChara,
        ) where

import qualified PhiMap as PM
import qualified Chara as C


data PlayerCharacter = PlayerCharacter {
  pcPosition :: PM.Position,
  pcDirection :: PM.AbsoluteDirection,
  pcName :: String} deriving (Show)
--  pcPhirc :: String} deriving (Show)


instance C.Chara PlayerCharacter where
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
--  changePosition pos chara = PlayerChara {pcPosition = pos, pcDirection = pcDirection chara, pcName = pcName chara, pcPhirc = pcPhirc chara}
  changePosition pos chara = PlayerCharacter {pcPosition = pos, pcDirection = pcDirection chara, pcName = pcName chara}
  getPosition chara = pcPosition chara
  getDirection chara = pcDirection chara
  getName chara = pcName chara
  getCharaView dir (x, y, chara) = C.CharaView
    {C.viewX = x, C.viewY = y,
     C.viewDirection = PM.calculateRelativeDirection dir $ C.getDirection chara, C.viewName = C.getName chara}
--  getPhirc chara = Just (pcPhirc chara)

--makePlayerChara :: PM.PhiMap -> String -> String -> PlayerChara
makePlayerChara :: PM.Position -> PM.AbsoluteDirection -> String -> PlayerCharacter
--makePlayerChara phi_map name phirc= PlayerChara {
makePlayerChara pos dir name = PlayerCharacter {
  pcPosition = pos,
  pcDirection = dir,
  pcName = name}
--  pcPhirc = phirc}
