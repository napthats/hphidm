module ProtocolDecoder
       (
         ClientProtocol(..),
         decodeClientMessage,
       ) where

import Data.String.Utils (split)
import qualified PhiMap as PM


data ClientProtocol = Open String | Go (Maybe PM.Direction) | UnknownProtocol

decodeClientMessage :: String -> ClientProtocol
decodeClientMessage msg =
  case msg of
    '#' : msg_rest -> case split " " msg_rest of
      ["open", phirc] -> Open phirc
      _ -> UnknownProtocol
    raw_message -> case split " " raw_message of
      ["go", dir] -> Go (stringToDirection dir)
      _ -> UnknownProtocol


stringToDirection :: String -> Maybe PM.Direction
stringToDirection "f" = Just (PM.RelativeDirection PM.Forth)
stringToDirection _ = Nothing
