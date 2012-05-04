module NonPlayerCharacter
       (
         NonPlayerCharacter(),
         NpcAction(..),
         NpcId,
         chooseAction,
         newNpcId,
         nextNpcId,
         makeNonPlayerCharacter,
         addLiveTime,
         getNpcId,
       ) where

import qualified PhiMap as PM
import qualified Chara as C


newtype NpcId = NpcId Integer deriving (Eq, Show)

newNpcId :: NpcId
newNpcId = NpcId 0

nextNpcId :: NpcId -> NpcId
nextNpcId (NpcId x) = NpcId $ x + 1


data NpcAction = RandomMove deriving (Show)

chooseAction :: NonPlayerCharacter -> NpcAction
chooseAction _ = RandomMove


data NonPlayerCharacter = NonPlayerCharacter {
  npcPosition :: PM.Position,
  npcDirection :: PM.AbsoluteDirection,
  npcName :: String,
  npcMillisecondsPerAction :: Int,
  npcLivetime :: Int,
  npcId :: NpcId} deriving (Show)

instance C.Chara NonPlayerCharacter where
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
  changePosition pos npc = NonPlayerCharacter {npcPosition = pos, npcDirection = npcDirection npc, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = npcLivetime npc, npcId = npcId npc}
  changeDirection dir npc = NonPlayerCharacter {npcPosition = npcPosition npc, npcDirection = dir, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = npcLivetime npc, npcId = npcId npc}
  getPosition npc = npcPosition npc
  getDirection npc = npcDirection npc
  getName npc = npcName npc
  getCharaView dir (x, y, npc) =
    C.CharaView x y (PM.calculateRelativeDirection dir $ C.getDirection npc) (C.getName npc)
  getSight phimap npc =
   PM.getVisiblePositions PM.All phimap (C.getPosition npc) (C.getDirection npc) sightWidth sightHeight

makeNonPlayerCharacter ::
  PM.Position -> PM.AbsoluteDirection -> String -> Int -> NpcId -> NonPlayerCharacter
makeNonPlayerCharacter pos dir name speed nid =
  NonPlayerCharacter {
    npcPosition = pos,
    npcDirection = dir,
    npcName = name,
    npcMillisecondsPerAction = speed,
    npcLivetime = 0,
    npcId = nid
  }

getNpcId :: NonPlayerCharacter -> NpcId
getNpcId npc = npcId npc


-- add livetime (milliseconds)
-- return new npc and whether npc ready to act or not
addLiveTime :: Int -> NonPlayerCharacter -> (NonPlayerCharacter, Bool)
addLiveTime dtime npc =
  let speed = npcMillisecondsPerAction npc in
  let livetime = npcLivetime npc in
  let new_livetime = max 0 (livetime + dtime) in -- livetime turns to 0 if overflow occurs
  let new_npc = NonPlayerCharacter {npcPosition = npcPosition npc, npcDirection = npcDirection npc, npcName = npcName npc, npcMillisecondsPerAction = npcMillisecondsPerAction npc, npcLivetime = new_livetime, npcId = npcId npc} in
  if (livetime `mod` speed) + dtime >= speed then (new_npc, True) else (new_npc, False)

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
