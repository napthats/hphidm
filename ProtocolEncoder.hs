module ProtocolEncoder
       (
         encodeProtocol,
         ServerProtocol(..),
         ObjType(..),
        ) where

import Data.Char (chr)
--import qualified Chara as PC
import qualified PhiMap as PM


data ServerProtocol =
  M57Map PM.AbsoluteDirection PM.PhiMapView
--  | M57Obj ObjType ObjId Int Int PM.AbsoluteDirection String GraphicStatus String GiantFlag GraphicType
  | M57Obj ObjType Int Int PM.RelativeDirection String
  | M57End
  | Close
  | X
  deriving (Show)
data ObjType = BObj | CObj | FObj deriving (Show)
--type ObjId = Int
--data GraphicStatus = Command
--type GiantFlag = Bool
--data GraphicType = Command

encodeProtocol :: ServerProtocol -> String
encodeProtocol (M57Map adir map_view) = makeM57MapProtocol adir map_view
encodeProtocol (M57Obj otype x y adir name) = makeM57ObjectProtocol otype x y adir name
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


-- tentative
-- x and y must be lower than 10
makeM57ObjectProtocol :: ObjType -> Int -> Int -> PM.RelativeDirection -> String -> String
makeM57ObjectProtocol otype x y adir name =
  if x >= 10 || y >= 10
     then error "view map size must be lower than 10."
     else let otype_string = case otype of
                BObj -> "B"
                CObj -> "C"
                FObj -> "F"
          in let dir_string = case adir of
                   PM.Forth -> "F"
                   PM.Right -> "R"
                   PM.Left -> "L"
                   PM.Back -> "B"
          in "#m57 O " ++ otype_string ++ "0000:" ++ show x ++ " " ++ show y ++ " " ++
             dir_string ++ " " ++ replicate (31 - length name) ' ' ++ take 31 name ++
             " 00                 # 00"
