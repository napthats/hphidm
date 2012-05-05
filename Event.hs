module Event
       (
         TriggeredEvent,
         EventTrigger(..),
         getTriggeredEvent,
       ) where

--
import Data.Maybe (fromJust)
--
import qualified PhiWorldData as PWD
import qualified PhiMap as PM
import qualified Chara as CH
import qualified PlayerCharacter as PC
--import qualified NonPlayerCharacter as NPC

data EventTrigger = PcPositionChange PM.PhiMap PC.PlayerCharacter deriving (Show)

type TriggeredEvent = PWD.ActionResult

getTriggeredEvent :: EventTrigger -> [TriggeredEvent]
getTriggeredEvent (PcPositionChange phimap pc) =
  if CH.getPosition pc == fromJust (PM.loadPosition phimap "0:0")
     then [PWD.PcStatusChange PWD.PSCDirection (PC.getPhirc pc)
          ((\c -> Just $ CH.changePosition (fromJust $ PM.loadPosition phimap "3:0") c))]
     else []
