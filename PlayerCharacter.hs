module PlayerCharacter
       (
         PlayerCharacter(),
         makePlayerCharacter,
         getPhirc,
        ) where

import qualified PhiMap as PM
import qualified Chara as C


data PlayerCharacter = PlayerCharacter {
  pcPosition :: PM.Position,
  pcDirection :: PM.AbsoluteDirection,
  pcName :: String,
  pcPhirc :: String} deriving (Show)

instance C.Chara PlayerCharacter where
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
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

makePlayerCharacter :: PM.Position -> PM.AbsoluteDirection -> String -> String-> PlayerCharacter
makePlayerCharacter pos dir name phirc = PlayerCharacter {
  pcPosition = pos,
  pcDirection = dir,
  pcName = name,
  pcPhirc = phirc}

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
