module ProtocolEncoder
       (
         makeM57MapProtocol,
        ) where

import Data.Char (chr)
--import qualified Chara as PC
import qualified PhiMap as PM


makeM57MapProtocol :: PM.AbsoluteDirection -> PM.PhiMapView -> String
makeM57MapProtocol adir map_view =
  "#m57 M " ++ adirToString adir ++ "        0:"
  ++foldl (\line_x line_y -> line_x ++ (foldl (\x y -> x ++ viewChipToString y) "" line_y)) "" map_view

adirToString :: PM.AbsoluteDirection -> String
adirToString PM.North = "N"
adirToString PM.East = "E"
adirToString PM.West = "W"
adirToString PM.South = "S"

viewChipToString :: PM.ViewChip -> String
viewChipToString chip =
  case PM.viewType chip of
    PM.VBars -> "I"
    PM.VDoor -> "["
    PM.VDummy -> "o"
    PM.VFlower -> "+"
    PM.VGlass -> "="
    PM.VGrass -> ":"
    PM.VMist -> "/"
    PM.VMwall -> "H"
    PM.VPcircle -> "x"
    PM.VRoad -> " "
    PM.VRock -> "@"
    PM.VTgate -> ">"
    PM.VUnknown -> "?"
    PM.VWater -> "_"
    PM.VWindow -> "|"
    PM.VWood -> "T"
    PM.VWwall -> "#"
    PM.VDoor_Lock -> "{"
    PM.VPcircle_Lock -> "%"
  ++ [chr 128]
