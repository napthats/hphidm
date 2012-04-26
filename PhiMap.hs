module PhiMap
       (
         PhiMap(),
         Direction(..),
         AbsoluteDirection(..),
         RelativeDirection(..),
         Position(),
         PhiMapChip(),
         PhiMapView(..),
         ViewType(..),
         ViewOption(..),
         FloorItemType(..),
         SightType(..),
         getMapView,
         getPhiMapChip,
         getNextPosition,
         getDefaultPosition,
         makePhiMap,
         turnAbsoluteDirection,
         isNormalEnterable,
        ) where

import Prelude hiding (Right, Left)
import Data.List (transpose)


data Direction = AbsoluteDirection AbsoluteDirection | RelativeDirection RelativeDirection
               deriving (Show)
data AbsoluteDirection = North | East | West | South deriving (Show)
data RelativeDirection = Forth | Right | Left | Back deriving (Show)
data Position = Position {x :: Int, y :: Int} deriving (Show)

isValidPosition :: PhiMap -> Position -> Bool
isValidPosition phi_map pos =
  if (x pos >= 0 && x pos < mapWidth phi_map && y pos >= 0 && y pos < mapHeight phi_map)
     then True else False

data PhiMap = PhiMap {mapWidth :: Int, mapHeight :: Int, mapData :: [[PhiMapChip]]} deriving (Show)
data PhiMapChip = PhiMapChip {chipType :: ChipType} deriving (Show)
data ChipType = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall deriving (Show)

outsidePhiMapChip :: PhiMapChip
outsidePhiMapChip = PhiMapChip {chipType = Unknown}

getPhiMapChip :: PhiMap -> Position -> PhiMapChip
getPhiMapChip phi_map pos = 
  if isValidPosition phi_map pos
  then (mapData phi_map) !! (x pos) !! (y pos)
  else outsidePhiMapChip

getPhiMapChipRegion :: PhiMap -> Position -> Int -> Int -> [[PhiMapChip]]
getPhiMapChipRegion phi_map pos width height =
  let map_data = mapData phi_map in
  map ((padTake width outsidePhiMapChip) . (padDrop (x pos) outsidePhiMapChip))
  (padTake height (replicate width outsidePhiMapChip) $ padDrop (y pos) (replicate width outsidePhiMapChip) map_data)

padTake :: Int -> a -> [a] -> [a]
padTake num pad list =
  (take num list) ++ (replicate (num - length list) pad)

padDrop :: Int -> a -> [a] -> [a]
padDrop num pad list =
  (replicate (- num) pad) ++ (drop num list)
  
data PhiMapView = PhiMapView {viewWidth :: Int, viewHeight :: Int, viewData :: [[ViewChip]]} deriving (Show)
data ViewChip = ViewChip {viewType :: ViewType, viewOptions :: [ViewOption]} deriving (Show)
data ViewType = VBars | VDoor | VDummy | VFlower | VGlass | VGrass | VMist | VMwall | VPcircle | VRoad | VRock | VTgate | VUnknown | VWater | VWindow | VWood | VWwall | VDoor_Lock | VPcircle_Lock deriving (Show)
data ViewOption = Board | FloorItem FloorItemType deriving (Show)
data FloorItemType = Food | Weapon | Armor | Accessary | Gold | Other deriving (Show)
data SightType = All deriving (Show)


getMapView :: PhiMap -> Position -> AbsoluteDirection -> Int -> Int -> SightType -> PhiMapView
getMapView phi_map pos adir width height All =
  let fixed_pos = case adir of
        North -> Position {x = x pos - (width - 1) `div` 2, y = y pos - 1 - (height - 1) `div` 2}
        East -> Position {x = x pos + 1 - (width - 1) `div` 2, y = y pos - (height - 1) `div` 2}
        West -> Position {x = x pos - 1 - (width - 1) `div` 2, y = y pos - (height - 1) `div` 2}
        South -> Position {x = x pos - (width - 1) `div` 2, y = y pos + 1 - (height - 1) `div` 2}
  in let map_data = case adir of
           North -> getPhiMapChipRegion phi_map fixed_pos width height
           South -> map reverse $ reverse $ getPhiMapChipRegion phi_map fixed_pos width height
           East -> reverse $ transpose $ getPhiMapChipRegion phi_map fixed_pos height width
           West -> map reverse $ transpose $ getPhiMapChipRegion phi_map fixed_pos height width
  in PhiMapView {viewWidth = width, viewHeight = height,
                 viewData = map (map mapChipToViewChip) map_data}

mapChipToViewChip :: PhiMapChip -> ViewChip
mapChipToViewChip map_chip =
  let view_type = case chipType map_chip of
        Bars -> VBars
        Door -> VDoor
        Dummy -> VDummy
        Flower -> VFlower
        Glass -> VGlass
        Grass -> VGlass
        Mist -> VMist
        Mwall -> VMwall
        Pcircle -> VPcircle
        Road -> VRoad
        Rock -> VRock
        Tgate -> VTgate
        Unknown -> VUnknown
        Water -> VWater
        Window -> VWindow
        Wood -> VWood
        Wwall -> VWwall
  in ViewChip {viewType = view_type, viewOptions = []}
  
getNextPosition :: PhiMap -> Position -> AbsoluteDirection -> Maybe Position
getNextPosition phi_map pos adir =
  let next_pos = case adir of
        North -> Position {x = x pos, y = y pos - 1}
        East -> Position {x = x pos + 1, y = y pos}
        West -> Position {x = x pos - 1, y = y pos}
        South -> Position {x = x pos, y = y pos + 1}
  in if isValidPosition phi_map next_pos then Just next_pos else Nothing

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
makePhiMap = PhiMap {mapWidth = 1000, mapHeight = 1000,
                     mapData = replicate 1000 $ replicate 1000 $ PhiMapChip {chipType = Road}}

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
