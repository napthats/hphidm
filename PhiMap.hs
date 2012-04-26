module PhiMap
       (
         Direction(..),
         AbsoluteDirection(..),
         RelativeDirection(..),
         PhiMap(),
         Position(),
         PhiMapChip(),
         getPhiMapChip,
         getNextPosition,
         getDefaultPosition,
         makePhiMap,
         turnAbsoluteDirection,
         isNormalEnterable,
        ) where

import Prelude hiding (Right, Left)


data Direction = AbsoluteDirection AbsoluteDirection | RelativeDirection RelativeDirection
               deriving (Show)
data AbsoluteDirection = North | East | West | South deriving (Show)
data RelativeDirection = Forth | Right | Left | Back deriving (Show)
data Position = Position {x :: Int, y :: Int} deriving (Show)
data PhiMap = PhiMap {width :: Int, height :: Int, mapData :: [PhiMapChip]} deriving (Show)
data PhiMapChip = PhiMapChip {chipType :: ChipType} deriving (Show)
data ChipType = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall deriving (Show)

validatePosition :: PhiMap -> Position -> Maybe Position
validatePosition phi_map pos =
  if (x pos >= 0 && x pos < width phi_map && y pos >= 0 && y pos < height phi_map)
     then Just pos else Nothing

getNextPosition :: PhiMap -> Position -> AbsoluteDirection -> Maybe Position
getNextPosition phi_map pos adir =
  case adir of
    North -> validatePosition phi_map $ Position {x = x pos, y = y pos - 1}
    East -> validatePosition phi_map $ Position {x = x pos + 1, y = y pos}
    West -> validatePosition phi_map $ Position {x = x pos - 1, y = y pos}
    South -> validatePosition phi_map $ Position {x = x pos, y = y pos + 1}

getPhiMapChip :: PhiMap -> Position -> PhiMapChip
getPhiMapChip phi_map pos = (mapData phi_map) !! (x pos + (y pos) * (width phi_map))

isNormalEnterable :: PhiMapChip -> Bool
isNormalEnterable chip = case chipType chip of
  Door -> True
  Dummy -> True
  Flower -> True
  Grass -> True
  Mist -> True
  Pcircle -> True
  Road -> True
  Tgate -> True
  Water -> True
  Bars -> False
  Glass -> False
  Mwall -> False
  Rock -> False
  Unknown -> False
  Window -> False
  Wood -> False
  Wwall -> False

getDefaultPosition :: PhiMap -> Position
getDefaultPosition _ = Position {x = 0, y = 0}

makePhiMap :: PhiMap
makePhiMap = PhiMap {width = 10, height = 10, mapData = replicate 100 $ PhiMapChip {chipType = Road}}

turnAbsoluteDirection :: AbsoluteDirection -> RelativeDirection -> AbsoluteDirection
turnAbsoluteDirection adir Forth = adir
turnAbsoluteDirection North Right = East
turnAbsoluteDirection North Left = West
turnAbsoluteDirection North Back = South
turnAbsoluteDirection East Right = South
turnAbsoluteDirection East Left = North
turnAbsoluteDirection East Back = West
turnAbsoluteDirection West Right = North
turnAbsoluteDirection West Left = South
turnAbsoluteDirection West Back = East
turnAbsoluteDirection South Right = West
turnAbsoluteDirection South Left = East
turnAbsoluteDirection South Back = North
