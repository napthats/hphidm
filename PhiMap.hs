module PhiMap
       (
         PhiMap(),
         PhiMapView,
         Direction(..),
         AbsoluteDirection(..),
         RelativeDirection(..),
         Position(),
         PhiMapChip(),
         ViewChip(..),
         ViewType(..),
         ViewOption(..),
         FloorItemType(..),
         SightType(..),
         getMapView,
         getVisiblePositions,
         getPhiMapChip,
         getNextPosition,
         getNextValidPosition,
         getDefaultPosition,
         makePhiMap,
         turnAbsoluteDirection,
         calculateRelativeDirection,
         isNormalEnterable,
         loadPosition,
         storePosition,
         addItem,
         deleteItem,
         getItemList,
        ) where

import Prelude hiding (Right, Left)
import Data.List (transpose)
import Data.String.Utils (split)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Item as IT


data Position = Position {x :: Int, y :: Int} deriving (Show, Eq, Ord)

isValidPosition :: PhiMap -> Position -> Bool
isValidPosition phimap pos =
  if (x pos >= 0 && x pos < mapWidth phimap && y pos >= 0 && y pos < mapHeight phimap)
     then True else False

-- return Position if format of str is "x:y" and (x, y) is valid Position
loadPosition :: PhiMap -> String -> Maybe Position
loadPosition phimap str = case split ":" str of
  [x_str, y_str] -> case reads x_str of
    [(x_num, "")] -> case reads y_str of
      [(y_num, "")] -> let pos = Position {x = x_num, y = y_num} in
        if isValidPosition phimap pos then Just pos else Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

storePosition :: PhiMap -> Position -> Maybe String
storePosition phimap pos =
  if isValidPosition phimap pos then Just $ show (x pos) ++ ":" ++ show (y pos) else Nothing

data Direction = AbsoluteDirection AbsoluteDirection | RelativeDirection RelativeDirection
               deriving (Show)
data AbsoluteDirection = North | East | West | South deriving (Show)
data RelativeDirection = Forth | Right | Left | Back deriving (Show)

calculateRelativeDirection :: AbsoluteDirection -> AbsoluteDirection -> RelativeDirection
calculateRelativeDirection North North = Back
calculateRelativeDirection North East = Right
calculateRelativeDirection North West = Left
calculateRelativeDirection North South = Forth
calculateRelativeDirection East North = Left
calculateRelativeDirection East East = Back
calculateRelativeDirection East West = Forth
calculateRelativeDirection East South = Right
calculateRelativeDirection West North = Right
calculateRelativeDirection West East = Forth
calculateRelativeDirection West West = Back
calculateRelativeDirection West South = Left
calculateRelativeDirection South North = Forth
calculateRelativeDirection South East = Left
calculateRelativeDirection South West = Right
calculateRelativeDirection South South = Back

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


data PhiMap = PhiMap {mapWidth :: Int, mapHeight :: Int, mapData :: [[(Position, PhiMapChip)]], itemDB :: Map.Map Position [IT.Item]} deriving (Show)
data PhiMapChip = PhiMapChip {chipType :: ChipType} deriving (Show)
data ChipType = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall deriving (Show)

outsidePhiMapChip :: (Position, PhiMapChip)
outsidePhiMapChip = (Position {x = -1, y = -1}, PhiMapChip {chipType = Unknown}) --tentative

getPhiMapChip :: PhiMap -> Position -> PhiMapChip
getPhiMapChip phimap pos = 
  if isValidPosition phimap pos
  then snd $ (mapData phimap) !! (y pos) !! (x pos)
  else snd outsidePhiMapChip

getPositionRegion :: PhiMap -> Position -> Int -> Int -> [[Position]]
getPositionRegion _ pos width height =
  take height $ iterate (map (\p -> Position {x = x p, y = y p + 1})) $
  take width $ iterate (\p -> Position {x = x p + 1, y = y p}) pos

getPhiMapChipRegion :: PhiMap -> Position -> Int -> Int -> [[(Position, PhiMapChip)]]
getPhiMapChipRegion phimap pos width height =
  let map_data = mapData phimap in
  map ((padTake width outsidePhiMapChip) . (padDrop (x pos) outsidePhiMapChip))
  (padTake height (replicate width outsidePhiMapChip) $ padDrop (y pos) (replicate width outsidePhiMapChip) map_data)

padTake :: Int -> a -> [a] -> [a]
padTake num pad list =
  (take num list) ++ (replicate (num - length list) pad)

padDrop :: Int -> a -> [a] -> [a]
padDrop num pad list =
  (replicate (- num) pad) ++ (drop num list)
  

type PhiMapView = [[ViewChip]]
data ViewChip = ViewChip {viewType :: ViewType, viewOptions :: [ViewOption]} deriving (Show)
data ViewType = VBars | VDoor | VDummy | VFlower | VGlass | VGrass | VMist | VMwall | VPcircle | VRoad | VRock | VTgate | VUnknown | VWater | VWindow | VWood | VWwall | VDoor_Lock | VPcircle_Lock deriving (Show)
data ViewOption = Board | FloorItem FloorItemType deriving (Show)
data FloorItemType = Food | Weapon | Armor | Accessary | Gold | Other deriving (Show)
data SightType = All deriving (Show)


getRegionWith :: (PhiMap -> Position -> Int -> Int -> [[a]]) ->
                 PhiMap -> Position -> AbsoluteDirection -> Int -> Int -> [[a]]
getRegionWith get_region_func phimap pos adir width height =
  let fixed_pos = case adir of
        North -> Position {x = x pos - (width - 1) `div` 2, y = y pos - 1 - (height - 1) `div` 2}
        East -> Position {x = x pos + 1 - (width - 1) `div` 2, y = y pos - (height - 1) `div` 2}
        West -> Position {x = x pos - 1 - (width - 1) `div` 2, y = y pos - (height - 1) `div` 2}
        South -> Position {x = x pos - (width - 1) `div` 2, y = y pos + 1 - (height - 1) `div` 2}
  in case adir of
    North -> get_region_func phimap fixed_pos width height
    South -> map reverse $ reverse $ get_region_func phimap fixed_pos width height
    East -> reverse $ transpose $ get_region_func phimap fixed_pos height width
    West -> map reverse $ transpose $ get_region_func phimap fixed_pos height width

getVisiblePositions ::
  SightType -> PhiMap -> Position -> AbsoluteDirection -> Int -> Int -> [[Position]]
getVisiblePositions All phimap pos adir width height =
  getRegionWith getPositionRegion phimap pos adir width height

getMapView :: SightType -> PhiMap -> Position -> AbsoluteDirection -> Int -> Int -> PhiMapView
getMapView All phimap pos adir width height =
  map (map (\(each_pos, map_chip) -> mapChipToViewChip phimap each_pos map_chip)) $
      getRegionWith getPhiMapChipRegion phimap pos adir width height

mapChipToViewChip :: PhiMap -> Position -> PhiMapChip -> ViewChip
mapChipToViewChip phimap pos map_chip =
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
        Wwall -> VWwall in
  -- tentative
  let view_option_list = if length (getItemList phimap pos) == 0 then [] else [FloorItem Other] in
  ViewChip {viewType = view_type, viewOptions = view_option_list}
  
-- can return invalid Position (for example, outside of tha map)
getNextPosition :: PhiMap -> Position -> AbsoluteDirection -> Position
getNextPosition _ pos adir =
  case adir of
        North -> Position {x = x pos, y = y pos - 1}
        East -> Position {x = x pos + 1, y = y pos}
        West -> Position {x = x pos - 1, y = y pos}
        South -> Position {x = x pos, y = y pos + 1}

getNextValidPosition :: PhiMap -> Position -> AbsoluteDirection -> Maybe Position
getNextValidPosition phimap pos adir =
  let next_pos = case adir of
        North -> Position {x = x pos, y = y pos - 1}
        East -> Position {x = x pos + 1, y = y pos}
        West -> Position {x = x pos - 1, y = y pos}
        South -> Position {x = x pos, y = y pos + 1} in
  if isValidPosition phimap next_pos then Just next_pos
  else Nothing


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


addItem :: Position -> IT.Item -> PhiMap -> Maybe PhiMap
addItem pos item phimap =
  if isValidPosition phimap pos
  then let itemdb = itemDB phimap in
       let new_itemdb = Map.alter (\maybe_items -> case maybe_items of
                                      Nothing -> Just [item]
                                      Just items -> Just (item : items)
                                  ) pos itemdb in
       Just $ PhiMap {mapWidth = mapWidth phimap, mapHeight = mapHeight phimap,
                      mapData = mapData phimap, itemDB = new_itemdb}
  else Nothing

deleteItem :: Position -> Int -> PhiMap -> Maybe (PhiMap, IT.Item)
deleteItem pos ord phimap =
  if ord < 0 || not (isValidPosition phimap pos) then Nothing
  else case Map.lookup pos (itemDB phimap) of
    Nothing -> Nothing
    Just item_list ->
      if length item_list <= ord
         then Nothing
         else Just (PhiMap {mapWidth =mapWidth phimap, mapHeight = mapHeight phimap,
                            mapData = mapData phimap,
                            itemDB = Map.insert pos (take ord item_list ++ drop (ord+1) item_list)
                                     (itemDB phimap)},
                    item_list !! ord)  

getItemList :: PhiMap -> Position -> [IT.Item]
getItemList phimap pos =
  case Map.lookup pos (itemDB phimap) of
    Nothing -> []
    Just item_list -> item_list


-- tentative
makePhiMap :: PhiMap
makePhiMap = 
  let phimap = PhiMap {mapWidth = 5, mapHeight = 5,
                       mapData = [[(Position {x = 0, y = 0}, PhiMapChip {chipType = Dummy}),
                                  (Position {x = 1, y = 0}, PhiMapChip {chipType = Dummy}),
                                  (Position {x = 2, y = 0}, PhiMapChip {chipType = Water}),
                                  (Position {x = 3, y = 0}, PhiMapChip {chipType = Window}),
                                  (Position {x = 4, y = 0}, PhiMapChip {chipType = Wwall})],
                                 [(Position {x = 0, y = 1}, PhiMapChip {chipType = Road}),
                                  (Position {x = 1, y = 1}, PhiMapChip {chipType = Flower}),
                                  (Position {x = 2, y = 1}, PhiMapChip {chipType = Water}),
                                  (Position {x = 3, y = 1}, PhiMapChip {chipType = Window}),
                                  (Position {x = 4, y = 1}, PhiMapChip {chipType = Wwall})],
                                 [(Position {x = 0, y = 2}, PhiMapChip {chipType = Road}),
                                  (Position {x = 1, y = 2}, PhiMapChip {chipType = Flower}),
                                  (Position {x = 2, y = 2}, PhiMapChip {chipType = Water}),
                                  (Position {x = 3, y = 2}, PhiMapChip {chipType = Window}),
                                  (Position {x = 4, y = 2}, PhiMapChip {chipType = Wwall})],
                                 [(Position {x = 0, y = 3}, PhiMapChip {chipType = Road}),
                                  (Position {x = 1, y = 3}, PhiMapChip {chipType = Flower}),
                                  (Position {x = 2, y = 3}, PhiMapChip {chipType = Water}),
                                  (Position {x = 3, y = 3}, PhiMapChip {chipType = Window}),
                                  (Position {x = 4, y = 3}, PhiMapChip {chipType = Wwall})],
                                 [(Position {x = 0, y = 4}, PhiMapChip {chipType = Road}),
                                  (Position {x = 1, y = 4}, PhiMapChip {chipType = Flower}),
                                  (Position {x = 2, y = 4}, PhiMapChip {chipType = Water}),
                                  (Position {x = 3, y = 4}, PhiMapChip {chipType = Window}),
                                  (Position {x = 4, y = 4}, PhiMapChip {chipType = Wwall})]],
                       itemDB = Map.fromList []} in
  foldl (\cphimap (pos, item) -> fromJust $ addItem pos item cphimap) phimap
  [(fromJust $ loadPosition phimap "2:2",
    IT.makeItem IT.ITWeapon "test1" (IT.Forth 1) IT.Steel IT.Sword 1 IT.Fire 0 IT.EFNone IT.SENone),
   (fromJust $ loadPosition phimap "2:2",
    IT.makeItem IT.ITWeapon "test2" (IT.Forth 1) IT.Steel IT.Sword 2 IT.Fire 0 IT.EFNone IT.SENone),
   (fromJust $ loadPosition phimap "3:3",
    IT.makeItem IT.ITWeapon "test3" (IT.Forth 1) IT.Steel IT.Sword 3 IT.Fire 0 IT.EFNone IT.SENone)]
