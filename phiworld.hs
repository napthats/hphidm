module PhiWorld (
  PhiWorld,
  newPhiWorld,
  getPlayerPosition,
  canMovePosition,
  setPlayerPosition
  ) where

import Data.List

import ClientId


data PhiWorld = PhiWorld String
type Position = (Int, Int)

newPhiWorld :: PhiWorld
newPhiWorld = PhiWorld $ "#####\n" ++ "#...#\n" ++ "#...#\n" ++ "##.##\n" ++ " ### " 

mapWidth :: Int
mapWidth = 6

initialPos :: (Int, Int)
initialPos = (2, 3)

getPlayerPosition :: ClientId -> PhiWorld -> Position
getPlayerPosition cid (PhiWorld mapString) =
  let cidnum = getIdNum cid in
    case index mapString (show cidnum) of
      Just linerPos -> (linerPos `mod` mapWidth, linerPos `div` mapWidth)
      Nothing -> initialPos

canMovePosition :: Position -> PhiWorld -> Bool
canMovePosition (x, y) (PhiWorld mapString) = 
  let linerPos = x + y * mapWidth in
    mapString !! linerPos == '.'

setPlayerPosition :: ClientId -> Position -> Position -> PhiWorld -> (String, PhiWorld)
setPlayerPosition cid (oldx, oldy) (x, y) (PhiWorld mapString) = 
  let linerPos = x + y * mapWidth
      oldLinerPos = oldx + oldy * mapWidth
      cidnum = getIdNum cid
  in let newMapString = updateIndexOf (updateIndexOf mapString oldLinerPos ".") linerPos (show cidnum)
  in  (newMapString, PhiWorld newMapString)

index :: (Eq a, Num b) => [a] -> [a] -> Maybe b
index str substr
    | isPrefixOf substr str = Just 0
    | length str <= length substr = Nothing
    | otherwise = index xs substr >>= return . (+1)
    where _ : xs = str

updateIndexOf :: [a] -> Int -> [a] -> [a]
updateIndexOf list num elem = (take num list) ++ elem ++ (drop (num + 1) list)
