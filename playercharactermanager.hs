module PlayerCharacterManager
       (
         moveCharacter
       ) where

import PhiWorld
import Direction

data CharacterId = CharacterId Int

moveCharacter :: PhiWorld -> CharacterId -> Direction -> (String, PhiWorld)
moveCharacter = undefined
