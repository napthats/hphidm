module DmMessages
       (
         DmMessageType(..),
         makeDmMessage,
       ) where


data DmMessageType = GoNo
                   | TurnNo
                   | TurnBad
                   | Savedata
                   | Seeyou
                   | Dead
                   | Tryagain
                   | KillBy String String
                   | NoCharacter
                   | AccessAlready
                   | ChangeClientFail
                   | NoItemHere
                   | GetSelect
                   | Get String String
                   | GetBad
                   | NoItemInvestory
                   | PutSelect
                   | Put String String
                   | PutBad
                   | PutBadHere
                   | List [String]
                   | AttackHp String String String Int
                   | PcMessage String String
                     
makeDmMessage :: DmMessageType -> String
makeDmMessage GoNo = "DM > Can not go. "
makeDmMessage TurnNo = "DM > Can not turn. "
makeDmMessage TurnBad = "DM > Which direction? Type 'turn ????'. "
makeDmMessage Savedata = "  Saving data..  "
makeDmMessage Seeyou = "  See you next time.  "
makeDmMessage Dead = "DM > You are dead... "
makeDmMessage Tryagain = "  Try again.  "
makeDmMessage NoCharacter = " Your character is not here. "
makeDmMessage AccessAlready = " You accessed already. "
makeDmMessage ChangeClientFail = " Changing client is failed. "

makeDmMessage NoItemHere = "DM > Here is no item. "
makeDmMessage GetSelect = "DM > Input item number to get"
makeDmMessage (Get chara_name item_name) = "DM > " ++ chara_name ++ " gets " ++ item_name ++ ". "
makeDmMessage GetBad = "DM > Can not get such a thing. "
makeDmMessage NoItemInvestory = "DM > You do not have any items. "
makeDmMessage PutSelect = "DM > Input item number to put"
makeDmMessage (Put chara_name item_name) = "DM > " ++ chara_name ++ " puts " ++ item_name ++ ". "
makeDmMessage PutBad = "DM > You do not have such a thing. "
makeDmMessage PutBadHere = "DM > Can not put anything here. "

makeDmMessage (List str_list) =
  "#list\n" ++ 
  concat (map (\(ord, str) -> "[" ++ (if ord < 10 then " " else "") ++ show ord ++ "] " ++ str ++ "\n")
              (zip [(1::Int)..] str_list)) ++
  "#end-list"

makeDmMessage (PcMessage name msg) = name ++ " > " ++ msg
makeDmMessage (AttackHp name vsname method value) =
  "DM > " ++ name ++ " attacked to " ++ vsname ++ " by " ++ method
    ++ ". [/*color=-hp*/" ++ show value ++ "/*.*/ hp] "
makeDmMessage (KillBy name vsname) = "DM > " ++ name ++ " is killed by " ++ vsname ++ ". "
