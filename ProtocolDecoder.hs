module ProtocolDecoder
       (
         ClientProtocol(..),
         decodeClientMessage,
       ) where

import Data.String.Utils (split)
import qualified PhiMap as PM


data ClientProtocol = Open String
                    | Go (Maybe PM.Direction)
                    | Exit
                    | UnknownProtocol

decodeClientMessage :: String -> ClientProtocol
decodeClientMessage msg =
  case msg of
    '#' : msg_rest -> case split " " msg_rest of
      ["open", phirc] -> Open phirc
      _ -> UnknownProtocol
    raw_message -> case split " " raw_message of
      ["exit"] -> Exit
      ["go"] -> Go Nothing
      ["go", dir] -> Go (stringToDirection dir)
      _ -> UnknownProtocol


stringToDirection :: String -> Maybe PM.Direction
stringToDirection st = case head st of
  'n' -> Just (PM.AbsoluteDirection PM.North)
  'e' -> Just (PM.AbsoluteDirection PM.East)
  'w' -> Just (PM.AbsoluteDirection PM.West)
  's' -> Just (PM.AbsoluteDirection PM.South)
  'f' -> Just (PM.RelativeDirection PM.Forth)
  'r' -> Just (PM.RelativeDirection PM.Right)
  'l' -> Just (PM.RelativeDirection PM.Left)
  'b' -> Just (PM.RelativeDirection PM.Back)
  _ -> Nothing
