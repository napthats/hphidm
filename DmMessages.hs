module DmMessages
       (
         DmMessageType(..),
         makeDmMessage,
       ) where


data DmMessageType = GoNo
                   | TurnBad
                   | Savedata
                   | Seeyou
                   | NoCharacter
                   | AccessAlready
                   | ChangeClientFail
                   | PcMessage String String
                     
makeDmMessage :: DmMessageType -> String
makeDmMessage GoNo = "DM > Can not go. "
makeDmMessage TurnBad = "DM > Which direction? Type 'turn ????'. "
makeDmMessage Savedata = "  Saving data..  "
makeDmMessage Seeyou = "  See you next time.  "
makeDmMessage NoCharacter = " Your character is not here. "
makeDmMessage AccessAlready = " You accessed already. "
makeDmMessage ChangeClientFail = " Changing client is failed. "
makeDmMessage (PcMessage name msg) = name ++ " > " ++ msg