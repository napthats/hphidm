module ProtocolDecoder
       (
         ClientProtocol(..),
         SProtocolType(..),
         RProtocolType(..),
         decodeClientMessage,
       ) where

import Data.String.Utils (split, join)
import qualified PhiMap as PM


data ClientProtocol = SharpProtocol SProtocolType | RawProtocol RProtocolType deriving (Show)
  
data SProtocolType = Open String | UnknownProtocol deriving (Show)

data RProtocolType = Go PM.Direction
                   | Turn (Maybe PM.Direction)
                   | Exit
                   | Hit
                   | Get (Maybe String)
                   | Put (Maybe String)
                   | RawMessage String
                   deriving (Show)

decodeClientMessage :: String -> ClientProtocol
decodeClientMessage msg =
  case msg of
    '#' : msg_rest -> case split " " msg_rest of
      ["open", phirc] -> SharpProtocol $ Open phirc
      _ -> SharpProtocol UnknownProtocol
    raw_message -> case split " " raw_message of
      ["exit"] -> RawProtocol Exit
      ["go"] -> RawProtocol $ Go $ PM.RelativeDirection PM.Forth
      ["go", dir] -> case stringToDirection dir of
                          Nothing -> RawProtocol $ Go $ PM.RelativeDirection PM.Forth
                          Just d -> RawProtocol $ Go d
      ["turn"] -> RawProtocol $ Turn Nothing
      ["turn", dir] -> RawProtocol $ Turn $ stringToDirection dir
      ["hit"] -> RawProtocol Hit
      ["get"] -> RawProtocol $ Get Nothing
      "get" : item_name_list -> RawProtocol $ Get $ Just $ join " " item_name_list
      ["put"] -> RawProtocol $ Put Nothing
      "put" : item_name_list -> RawProtocol $ Put $ Just $ join " " item_name_list
      _ -> RawProtocol $ RawMessage raw_message

stringToDirection :: String -> Maybe PM.Direction
stringToDirection st = case st of
  'n':_ -> Just (PM.AbsoluteDirection PM.North)
  'e':_ -> Just (PM.AbsoluteDirection PM.East)
  'w':_ -> Just (PM.AbsoluteDirection PM.West)
  's':_ -> Just (PM.AbsoluteDirection PM.South)
  'f':_ -> Just (PM.RelativeDirection PM.Forth)
  'r':_ -> Just (PM.RelativeDirection PM.Right)
  'l':_ -> Just (PM.RelativeDirection PM.Left)
  'b':_ -> Just (PM.RelativeDirection PM.Back)
  _ -> Nothing
