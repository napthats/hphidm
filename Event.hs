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
  let pos = CH.getPosition pc in
  if pos == fromJust (PM.loadPosition phimap "0:0")
     then [PWD.PcStatusChange PWD.PSCPosition (PC.getPhirc pc)
          ((\c -> Just $ CH.changePosition (PM.getNextPosition phimap (CH.getPosition c) PM.East) c))]
     else if CH.getPosition pc == fromJust (PM.loadPosition phimap "1:0")
          then [PWD.PcStatusChange PWD.PSCPosition (PC.getPhirc pc)
                ((\c -> Just $ CH.changePosition (PM.getNextPosition phimap (CH.getPosition c) PM.West) c))]
          else []
