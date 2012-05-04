module DmMessages
       (
         DmMessageType(..),
         makeDmMessage,
       ) where


data DmMessageType = GoNo
                   | TurnBad
                   | Savedata
                   | Seeyou
                   | Dead
                   | Tryagain
                   | KillBy String String
                   | NoCharacter
                   | AccessAlready
                   | ChangeClientFail
                   | AttackHp String String String Int
                   | PcMessage String String
                     
makeDmMessage :: DmMessageType -> String
makeDmMessage GoNo = "DM > Can not go. "
makeDmMessage TurnBad = "DM > Which direction? Type 'turn ????'. "
makeDmMessage Savedata = "  Saving data..  "
makeDmMessage Seeyou = "  See you next time.  "
makeDmMessage Dead = "DM > You are dead... "
makeDmMessage Tryagain = "  Try again.  "
makeDmMessage NoCharacter = " Your character is not here. "
makeDmMessage AccessAlready = " You accessed already. "
makeDmMessage ChangeClientFail = " Changing client is failed. "
makeDmMessage (PcMessage name msg) = name ++ " > " ++ msg
makeDmMessage (AttackHp name vsname method value) =
  "DM > " ++ name ++ " attacked to " ++ vsname ++ " by " ++ method
    ++ ". [/*color=-hp*/" ++ show value ++ "/*.*/ hp] "
makeDmMessage (KillBy name vsname) = "DM > " ++ name ++ " is killed by " ++ vsname ++ ". "
