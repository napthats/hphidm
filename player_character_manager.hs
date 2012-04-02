module PlayerCharacterManager
       (
         moveCharacter
       ) where

import PhiWorld
import Direction


data AbsoluteDirection = N | E | W | S

moveCharacter :: PhiWorld -> CharacterId -> AbsoluteDirection -> (String, PhiWorld)
moveCharacter = undefined
