module PhiWorldData
       (
         Phirc,
         ActionResult(..),
         PcStatusChangeType(..),
         NpcStatusChangeType(..),
        ) where

import qualified PlayerCharacter as PC
import qualified NonPlayerCharacter as NPC
import qualified Network.SimpleTCPServer as NS


type Phirc = String
data ActionResult = 
  NewPc NS.ClientID Phirc PC.PlayerCharacter
  | PcStatusChange PcStatusChangeType Phirc (PC.PlayerCharacter -> Maybe PC.PlayerCharacter)
  | NpcStatusChange NpcStatusChangeType NPC.NpcId (NPC.NonPlayerCharacter -> Maybe NPC.NonPlayerCharacter)
  | MessageFromDm NS.ClientID String
  | MessageFromPc PC.PlayerCharacter PC.PlayerCharacter String
  | LogoutPc NS.ClientID
  | PcHit PC.PlayerCharacter
  | ForceDisconnect NS.ClientID
data PcStatusChangeType = PSCDirection | PSCPosition deriving (Show)
data NpcStatusChangeType = NSCDirection | NSCPosition deriving (Show)
