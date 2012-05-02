module DmMessages
       (
         DmMessageType(..),
         makeDmMessage,
       ) where


data DmMessageType = GoNo
                   | Savedata
                   | Seeyou
                   | NoCharacter
                   | AccessAlready
                   | ChangeClientFail
                     
makeDmMessage :: DmMessageType -> String
makeDmMessage GoNo = "DM > Can not go. "
makeDmMessage Savedata = "  Saving data..  "
makeDmMessage Seeyou = "  See you next time.  "
makeDmMessage NoCharacter = " Your character is not here. "
makeDmMessage AccessAlready = " You accessed already. "
makeDmMessage ChangeClientFail = " Changing client is failed. "
