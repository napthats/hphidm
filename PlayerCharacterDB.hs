module PlayerCharacterDB
       (
         PlayerCharacterDB(),
         PC.PlayerCharacter(),
         loadPc,
         savePc,
         deletePcData,
         makePcDB,
       ) where

import Data.List.Utils (addToAL, delFromAL)
import qualified PlayerCharacter as PC
import qualified Chara as CH
import qualified PhiMap as PM


type Name = String
type PcData = (PM.Position, PM.AbsoluteDirection, Name)
type Phirc = String
newtype PlayerCharacterDB = PcDB [(Phirc, PcData)] deriving (Show)


loadPc :: PlayerCharacterDB -> Phirc -> Maybe PC.PlayerCharacter
loadPc (PcDB pcdb) phirc = case lookup phirc pcdb of
  Nothing -> Nothing
  Just (pos, dir, name) -> Just $ PC.makePlayerChara pos dir name

savePc :: PlayerCharacterDB -> Phirc -> PC.PlayerCharacter -> PlayerCharacterDB
savePc (PcDB pcdb) phirc pc =
  PcDB $ addToAL pcdb phirc (CH.getPosition pc, CH.getDirection pc, CH.getName pc)

deletePcData :: PlayerCharacterDB -> Phirc -> PlayerCharacterDB
deletePcData (PcDB pcdb) phirc = PcDB $ delFromAL pcdb phirc

-- test data
makePcDB :: PM.PhiMap -> IO PlayerCharacterDB
makePcDB phimap = return $ PcDB [("1", (case PM.loadPosition phimap "0:1" of
                                           Nothing -> undefined
                                           Just pos -> pos,
                                        PM.East,
                                        "test1")),
                                 ("2", (case PM.loadPosition phimap "2:2" of
                                           Nothing -> undefined
                                           Just pos -> pos,
                                        PM.South,
                                        "test2"))]