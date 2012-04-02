module ClientId
       (
         ClientId,
         getBroadcastId,
         getNewId,
         getNextId,
         getIdNum,
         getServerId
       ) where
       
newtype ClientId = ClientId Int
                   deriving (Show)
instance Eq ClientId where
  (ClientId x) == (ClientId y)
    | x < 0 = True
    | y < 0 = True
    | otherwise = x == y

getBroadcastId :: ClientId
getBroadcastId = ClientId (-1)

getServerId ::ClientId
getServerId = ClientId 0

getNewId :: ClientId
getNewId = ClientId 1

getNextId :: ClientId -> ClientId
getNextId (ClientId x) = ClientId (x + 1)

getIdNum :: ClientId -> Int
getIdNum (ClientId x) = x
