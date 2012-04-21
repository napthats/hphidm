module Hphidm.ServerMessage
       (
         ServerMessage(..),
       ) where


data ServerMessage = NoMessage | ToPcMessage String

