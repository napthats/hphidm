module Event
       (
         TriggeredEvent,
         EventTrigger(..),
         SwitchDB(),
         makeSwitchDB,
         getTriggeredEvent,
       ) where

--
import Data.Maybe (fromJust)
--
import qualified PhiWorldData as PWD
import qualified PhiMap as PM
import qualified Chara as CH
import qualified PlayerCharacter as PC
import qualified NonPlayerCharacter as NPC


newtype SwitchDB =
  SwitchDB ([(PcSwitchType, (PM.PhiMap -> PC.PlayerCharacter -> Bool,
                             PWD.PcStatusChangeType,
                             PM.PhiMap -> PC.PlayerCharacter -> Maybe PC.PlayerCharacter))],
            [(NpcSwitchType, (PM.PhiMap -> NPC.NonPlayerCharacter -> Bool,
                              PWD.NpcStatusChangeType,
                              PM.PhiMap -> NPC.NonPlayerCharacter -> Maybe NPC.NonPlayerCharacter))])

data EventTrigger = PcPositionChange PM.PhiMap PC.PlayerCharacter 
                  | NpcPositionChange PM.PhiMap NPC.NonPlayerCharacter
                    deriving (Show)

type TriggeredEvent = PWD.ActionResult

data PcSwitchType = PSPositionChange deriving (Show, Eq)
data NpcSwitchType = NSPositionChange deriving (Show, Eq)


getTriggeredEvent :: SwitchDB -> EventTrigger -> [TriggeredEvent]
getTriggeredEvent (SwitchDB swdb) (PcPositionChange phimap pc) =
  let switch_list = filter (\switch -> fst switch == PSPositionChange) (fst swdb) in      
  let triggered_switch_list = filter (\(_, (judge, _, _)) -> judge phimap pc) switch_list in
  map (\(_, (_, pcsct, change)) -> PWD.PcStatusChange pcsct (PC.getPhirc pc) (change phimap))
  triggered_switch_list
getTriggeredEvent (SwitchDB swdb) (NpcPositionChange phimap npc) =
  let switch_list = filter (\switch -> fst switch == NSPositionChange) (snd swdb) in
  let triggered_switch_list = filter (\(_, (judge, _, _)) -> judge phimap npc) switch_list in
  map (\(_, (_, npcsct, change)) -> PWD.NpcStatusChange npcsct (NPC.getNpcId npc) (change phimap))
  triggered_switch_list


makeSwitchDB :: SwitchDB
makeSwitchDB =
  SwitchDB ([(PSPositionChange,
             ((\phimap pc -> CH.getPosition pc == fromJust (PM.loadPosition phimap "0:0")),
              PWD.PSCPosition,
              (\phimap pc -> 
                case PM.getNextValidPosition phimap (CH.getPosition pc) PM.East of
                  Nothing -> Nothing
                  Just next_pos -> Just $ CH.changePosition next_pos pc))),
             (PSPositionChange,
             ((\phimap pc -> CH.getPosition pc == fromJust (PM.loadPosition phimap "1:0")),
              PWD.PSCPosition,
              (\phimap pc -> 
                case PM.getNextValidPosition phimap (CH.getPosition pc) PM.West of
                  Nothing -> Nothing
                  Just next_pos -> Just $ CH.changePosition next_pos pc)))],
             [(NSPositionChange,
             ((\phimap pc -> CH.getPosition pc == fromJust (PM.loadPosition phimap "0:0")),
              PWD.NSCPosition,
              (\phimap pc -> 
                case PM.getNextValidPosition phimap (CH.getPosition pc) PM.West of
                  Nothing -> Nothing
                  Just next_pos -> Just $ CH.changePosition next_pos pc)))])
