module PlayerCharacter
       (
         PlayerCharacter(),
         makePlayerCharacter,
         getPhirc,
        ) where

import qualified PhiMap as PM
import qualified Chara as CH
import qualified Combat as CO
import qualified Item as IT
import CharaData


instance CH.Chara PlayerCharacter where
  changePosition pos pc = PlayerCharacter {pcPosition = pos, pcDirection = pcDirection pc, pcName = pcName pc, pcPhirc = pcPhirc pc, pcMhp = pcMhp pc, pcHp = pcHp pc, pcMmp = pcMmp pc, pcMp = pcMp pc, pcInjuredBy = pcInjuredBy pc, pcItemList = pcItemList pc}
  changeDirection dir pc = PlayerCharacter {pcPosition = pcPosition pc, pcDirection = dir, pcName = pcName pc, pcPhirc = pcPhirc pc, pcMhp = pcMhp pc, pcHp = pcHp pc, pcMmp = pcMmp pc, pcMp = pcMp pc, pcInjuredBy = pcInjuredBy pc, pcItemList = pcItemList pc}
  addHp dhp injured_by pc = PlayerCharacter {pcPosition = pcPosition pc, pcDirection = pcDirection pc, pcName = pcName pc, pcPhirc = pcPhirc pc, pcMhp = pcMhp pc, pcHp = min (pcMhp pc) (pcHp pc + dhp), pcMmp = pcMmp pc, pcMp = pcMp pc, pcInjuredBy = Just injured_by, pcItemList = pcItemList pc}
  
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
  getPosition pc = pcPosition pc
  getDirection pc = pcDirection pc
  getName pc = pcName pc
  getMhp pc = pcMhp pc
  getHp pc = pcHp pc
  getMmp pc = pcMmp pc
  getMp pc = pcMp pc
  isDead pc = CH.getHp pc <= 0 || CH.getMp pc <= 0
  getCharaView dir (x, y, chara) =
    CH.CharaView x y (PM.calculateRelativeDirection dir $ CH.getDirection chara) (CH.getName chara)
  getSight phimap pc =
   PM.getVisiblePositions PM.All phimap (CH.getPosition pc) (CH.getDirection pc) sightWidth sightHeight
  getHitRange phimap pc = [PM.getNextPosition phimap (CH.getPosition pc) (CH.getDirection pc)]
  getLastInjured pc = pcInjuredBy pc
  
  hitTo pc vschara =
    let next_vschara = CH.addHp (-100) (IBPc pc) vschara in
    (pc, next_vschara, CO.Dummy (CH.getName pc) (CH.getName vschara) "Knuckle" (100))
--    (pc, next_vschara, CO.Dummy (CH.getName pc) (CH.getName vschara) "Knuckle" (CH.getHp next_vschara))
  
  addItem item pc =
    PlayerCharacter {pcPosition = pcPosition pc, pcDirection = pcDirection pc, pcName = pcName pc, pcPhirc = pcPhirc pc, pcMhp = pcMhp pc, pcHp = pcHp pc, pcMmp = pcMmp pc, pcMp = pcMp pc, pcInjuredBy = pcInjuredBy pc, pcItemList = pcItemList pc ++ [item]}

  deleteItem ord pc =
    if ord < 0 then Nothing
    else let item_list = pcItemList pc in
    if length item_list <= ord then Nothing
    else Just (PlayerCharacter {pcPosition = pcPosition pc, pcDirection = pcDirection pc, pcName = pcName pc, pcPhirc = pcPhirc pc, pcMhp = pcMhp pc, pcHp = pcHp pc, pcMmp = pcMmp pc, pcMp = pcMp pc, pcInjuredBy = pcInjuredBy pc, pcItemList = take ord item_list ++ drop (ord+1) item_list}, item_list !! ord)
  
  getItemList pc = pcItemList pc

getPhirc :: PlayerCharacter -> String
getPhirc pc = pcPhirc pc

makePlayerCharacter ::
  PM.Position -> PM.AbsoluteDirection -> String -> String -> Int -> Int -> Int -> Int -> [IT.Item] -> PlayerCharacter
makePlayerCharacter pos dir name phirc mhp hp mmp mp item_list = PlayerCharacter {
  pcPosition = pos,
  pcDirection = dir,
  pcName = name,
  pcPhirc = phirc,
  pcMhp = mhp,
  pcHp = min mhp hp,
  pcMmp = mmp,
  pcMp = min mmp mp,
  pcInjuredBy = Nothing,
  pcItemList = item_list}

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
