module NonPlayerCharacter
       (
         NonPlayerCharacter(),
         NpcAction(..),
         NpcId,
         chooseAction,
         newNpcId,
         nextNpcId,
         makeNonPlayerCharacter,
         addLivetime,
         canActNext,
         getNpcId,
       ) where

import qualified PhiMap as PM
import qualified Chara as CH
import CharaData




data NpcAction = RandomMove deriving (Show)

chooseAction :: NonPlayerCharacter -> NpcAction
chooseAction _ = RandomMove


instance CH.Chara NonPlayerCharacter where
  changePosition pos npc = NonPlayerCharacter {npcPosition = pos, npcDirection = npcDirection npc, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = npcLivetime npc, npcId = npcId npc, npcMhp = npcMhp npc, npcHp = npcHp npc, npcMmp = npcMmp npc, npcMp = npcMp npc, npcInjuredBy = npcInjuredBy npc}
  changeDirection dir npc = NonPlayerCharacter {npcPosition = npcPosition npc, npcDirection = dir, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = npcLivetime npc, npcId = npcId npc, npcMhp = npcMhp npc, npcHp = npcHp npc, npcMmp = npcMmp npc, npcMp = npcMp npc, npcInjuredBy = npcInjuredBy npc}
  addHp dhp injured_by npc = NonPlayerCharacter {npcPosition = npcPosition npc, npcDirection = npcDirection npc, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = npcLivetime npc, npcId = npcId npc, npcMhp = npcMhp npc, npcHp = min (npcMhp npc) (npcHp npc + dhp), npcMmp = npcMmp npc, npcMp = npcMp npc, npcInjuredBy = Just injured_by}
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
  getPosition npc = npcPosition npc
  getDirection npc = npcDirection npc
  getName npc = npcName npc
  getMhp npc = npcMhp npc
  getHp npc = npcHp npc
  getMmp npc = npcMmp npc
  getMp npc = npcMp npc
  isDead npc = CH.getHp npc <= 0 || CH.getMp npc <= 0
  getCharaView dir (x, y, npc) =
    CH.CharaView x y (PM.calculateRelativeDirection dir $ CH.getDirection npc) (CH.getName npc)
  getSight phimap npc =
    PM.getVisiblePositions PM.All phimap (CH.getPosition npc) (CH.getDirection npc)
    sightWidth sightHeight
  hitTo = undefined
  getHitRange = undefined
  getLastInjured npc = npcInjuredBy npc

makeNonPlayerCharacter ::
  PM.Position -> PM.AbsoluteDirection -> String -> Int -> NpcId -> Int -> Int -> Int -> Int -> NonPlayerCharacter
makeNonPlayerCharacter pos dir name speed nid mhp hp mmp mp =
  NonPlayerCharacter {
    npcPosition = pos,
    npcDirection = dir,
    npcName = name,
    npcMillisecondsPerAction = speed,
    npcLivetime = 0,
    npcId = nid,
    npcMhp = mhp,
    npcHp = min mhp hp,
    npcMmp = mmp,
    npcMp = min mmp mp,
    npcInjuredBy = Nothing
  }

getNpcId :: NonPlayerCharacter -> NpcId
getNpcId npc = npcId npc


-- add livetime (milliseconds)
addLivetime :: Int -> NonPlayerCharacter -> NonPlayerCharacter
addLivetime dtime npc =
  let livetime = npcLivetime npc in
  let new_livetime = max 0 (livetime + dtime) in -- livetime turns to 0 if overflow occurs
  NonPlayerCharacter {npcPosition = npcPosition npc, npcDirection = npcDirection npc, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = new_livetime, npcId = npcId npc, npcMhp = npcMhp npc, npcHp = npcHp npc, npcMmp = npcMmp npc, npcMp = npcMp npc, npcInjuredBy = npcInjuredBy npc}

-- return whether npc ready to act or not by next time (milliseconds)
canActNext :: Int -> NonPlayerCharacter -> Bool
canActNext dtime npc =
  let speed = npcMillisecondsPerAction npc in
  let livetime = npcLivetime npc in
  (livetime `mod` speed) + dtime >= speed

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
