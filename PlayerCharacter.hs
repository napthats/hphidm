module PlayerCharacter
       (
         PlayerCharacter(),
         makePlayerChara,
         getPhirc,
        ) where

import qualified PhiMap as PM
import qualified Chara as C


data PlayerCharacter = PlayerCharacter {
  pcPosition :: PM.Position,
  pcDirection :: PM.AbsoluteDirection,
  pcName :: String,
  pcPhirc :: String} deriving (Show)
--  pcPhirc :: String} deriving (Show)


instance C.Chara PlayerCharacter where
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
--  changePosition pos chara = PlayerChara {pcPosition = pos, pcDirection = pcDirection chara, pcName = pcName chara, pcPhirc = pcPhirc chara}
  changePosition pos chara = PlayerCharacter {pcPosition = pos, pcDirection = pcDirection chara, pcName = pcName chara, pcPhirc = pcPhirc chara}
  changeDirection dir chara = PlayerCharacter {pcPosition = pcPosition chara, pcDirection = dir, pcName = pcName chara, pcPhirc = pcPhirc chara}
  getPosition chara = pcPosition chara
  getDirection chara = pcDirection chara
  getName chara = pcName chara
  getCharaView dir (x, y, chara) =
    C.CharaView x y (PM.calculateRelativeDirection dir $ C.getDirection chara) (C.getName chara)
  getSight phimap pc =
    PM.getVisiblePositions PM.All phimap (C.getPosition pc) (C.getDirection pc) sightWidth sightHeight

getPhirc :: PlayerCharacter -> String
getPhirc pc = pcPhirc pc

--makePlayerChara :: PM.PhiMap -> String -> String -> PlayerChara
makePlayerChara :: PM.Position -> PM.AbsoluteDirection -> String -> String-> PlayerCharacter
--makePlayerChara phi_map name phirc= PlayerChara {
makePlayerChara pos dir name phirc = PlayerCharacter {
  pcPosition = pos,
  pcDirection = dir,
  pcName = name,
  pcPhirc = phirc}
--  pcPhirc = phirc}

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7