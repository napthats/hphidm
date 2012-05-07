module PhiWorldData
       (
         Phirc,
         ActionResult(..),
         PcStatusChangeType(..),
         NpcStatusChangeType(..),
         CharaInstance(..),
        ) where

import qualified PlayerCharacter as PC
import qualified NonPlayerCharacter as NPC
import qualified Network.SimpleTCPServer as NS


type Phirc = String
data ActionResult = 
  NewPc NS.ClientID Phirc
  | GetItem CharaInstance (Maybe String)
  | PutItem CharaInstance (Maybe String)
  | PcStatusChange PcStatusChangeType Phirc (PC.PlayerCharacter -> Maybe PC.PlayerCharacter)
  | NpcStatusChange NpcStatusChangeType NPC.NpcId (NPC.NonPlayerCharacter -> Maybe NPC.NonPlayerCharacter)
  | MessageFromDm NS.ClientID String
  | MessageFromPc PC.PlayerCharacter PC.PlayerCharacter String
  | LogoutPc NS.ClientID
  | PcHit PC.PlayerCharacter
  | ForceDisconnect NS.ClientID
data PcStatusChangeType = PSCDirection | PSCPosition deriving (Show)
data NpcStatusChangeType = NSCDirection | NSCPosition deriving (Show)

data CharaInstance = Pc PC.PlayerCharacter NS.ClientID | Npc NPC.NonPlayerCharacter deriving (Show)
