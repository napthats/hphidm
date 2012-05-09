module CharaData
       (
         PlayerCharacter(..),
         NonPlayerCharacter(..),
         InjuredBy(..),
         NpcId(..),
         PcState(..),
         SLAction(..),
         nextNpcId,
         newNpcId,
       ) where
import qualified PhiMap as PM
import qualified Item as IT


newtype NpcId = NpcId Integer deriving (Eq, Show, Ord)

newNpcId :: NpcId
newNpcId = NpcId 0

nextNpcId :: NpcId -> NpcId
nextNpcId (NpcId x) = NpcId $ x + 1


data NonPlayerCharacter = NonPlayerCharacter {
  npcPosition :: PM.Position,
  npcDirection :: PM.AbsoluteDirection,
  npcName :: String,
  npcMillisecondsPerAction :: Int,
  npcLivetime :: Int,
  npcId :: NpcId,
  npcMhp :: Int,
  npcHp :: Int,
  npcMmp :: Int,
  npcMp :: Int,
  npcInjuredBy :: Maybe InjuredBy} deriving (Show)



data PlayerCharacter = PlayerCharacter {
  pcPosition :: PM.Position,
  pcDirection :: PM.AbsoluteDirection,
  pcName :: String,
  pcPhirc :: String,
  pcMhp :: Int,
  pcHp :: Int,
  pcMmp :: Int,
  pcMp :: Int,
  pcInjuredBy :: Maybe InjuredBy,
  pcItemList :: [IT.Item],
  pcState :: PcState} deriving (Show)

data InjuredBy = IBPc PlayerCharacter 
               | IBNpc NonPlayerCharacter
               deriving (Show)

data PcState = Command | SelectList SLAction deriving (Show)

data SLAction = SLPut | SLGet | SLCast deriving (Show)
