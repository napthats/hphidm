module Combat
       (
         CombatResult(..),
         makeDmMessageTypeList,
       ) where

import qualified DmMessages as DM


data CombatResult = Dummy String String String Int deriving (Show)

makeDmMessageTypeList :: CombatResult -> [DM.DmMessageType]
makeDmMessageTypeList (Dummy name vsname method value) = [DM.AttackHp name vsname method value]
