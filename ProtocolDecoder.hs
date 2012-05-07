module ProtocolDecoder
       (
         ClientProtocol(..),
         decodeClientMessage,
       ) where

import Data.String.Utils (split)
import qualified PhiMap as PM


data ClientProtocol = Open String
                    | Go PM.Direction
                    | Turn (Maybe PM.Direction)
                    | Exit
                    | Hit
                    | Get (Maybe String)
                    | Put (Maybe String)
                    | RawMessage String
                    | UnknownProtocol
                    deriving (Show)

decodeClientMessage :: String -> ClientProtocol
decodeClientMessage msg =
  case msg of
    '#' : msg_rest -> case split " " msg_rest of
      ["open", phirc] -> Open phirc
      _ -> UnknownProtocol
    raw_message -> case split " " raw_message of
      ["exit"] -> Exit
      ["go"] -> Go $ PM.RelativeDirection PM.Forth
      ["go", dir] -> case stringToDirection dir of
                          Nothing -> Go $ PM.RelativeDirection PM.Forth
                          Just d -> Go d
      ["turn"] -> Turn Nothing
      ["turn", dir] -> Turn $ stringToDirection dir
      ["hit"] -> Hit
      ["get"] -> Get Nothing
      ["get", item_name] -> Get (Just item_name)
      ["put"] -> Put Nothing
      ["put", item_name] -> Put (Just item_name)
      _ -> RawMessage raw_message

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
