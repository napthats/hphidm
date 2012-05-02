module ProtocolEncoder
       (
         encodeProtocol,
         ServerProtocol(..),
        ) where

import Data.Char (chr)
--import qualified Chara as PC
import qualified PhiMap as PM


data ServerProtocol = M57Map PM.AbsoluteDirection PM.PhiMapView
                    | M57End
                    | Close
                    | X
                      deriving (Show)

encodeProtocol :: ServerProtocol -> String
encodeProtocol (M57Map adir map_view) = makeM57MapProtocol adir map_view
encodeProtocol Close = "#close"
encodeProtocol X = "#x"
encodeProtocol M57End = "#m57 ."


{-
makeM57MapProtocol :: PM.AbsoluteDirection -> PM.PhiMapView -> String
makeM57MapProtocol adir map_view =
  "#m57 M " ++ adirToString adir ++ "        0:"
  ++foldl (\line_x line_y -> line_x ++ (foldl (\x y -> x ++ viewChipToString y) "" line_y)) "" map_view
-}

-- for debug in telnet
makeM57MapProtocol :: PM.AbsoluteDirection -> PM.PhiMapView -> String
makeM57MapProtocol adir map_view =
  adirToString adir
  ++foldl (\line_x line_y -> line_x ++ (foldl (\x y -> x ++ viewChipToString y) "\n" line_y)) "" map_view ++ "\n"

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
