module ProtocolParser (
  parseProtocolString,
  Direction,
  Protocol
  ) where

import Direction


data Protocol = Go Direction
              | Turn Direction
              | Read
              | Board
              | Use
              | Erase
              | FloorItem
              | Guard
              | Check
              | Look
              | Pay (Maybe Int)
              | Equip
              | Spells
              | Write
              | Sort
              | Unequip
              | Put
--              | Y
              | Get
              | Hit
              | Cast
              | Dot
              | NormalMessage String

parseDirection :: String -> Direction
parseDirection str = case str of
  "n" -> N
  "N" -> N  
  "e" -> E
  "E" -> E
  "w" -> W
  "W" -> W
  "s" -> S
  "S" -> S
  "f" -> F
  "F" -> F
  "r" -> R
  "R" -> R
  "l" -> L
  "L" -> L
  "b" -> B
  "B" -> B
  _ -> F

parseProtocolString :: String -> Protocol
parseProtocolString str =
  let (protocolType : remain) = words str
  in case protocolType of
    "go" -> Go (parseDirection $ concat remain)
    "turn" -> Turn (parseDirection $ concat remain)
    "read" -> Read
    "board" -> Board
    "use" -> Use
    "erase" -> Erase
    "floor item" -> FloorItem
    "guard" -> Guard
    "check" -> Check
    "look" -> Look
    "pay" -> Pay (maybeRead $ concat remain)
    "equip" -> Equip
    "spells" -> Spells
    "write" -> Write
    "sort" -> Sort
    "unequip" -> Unequip
    "put" -> Put
    "get" -> Get
    "hit" -> Hit
    "cast" -> Cast
    "." -> Dot
    _ -> NormalMessage str

maybeRead :: String -> Maybe Int
maybeRead str = case reads str of
  [] -> Nothing
  (x, _) : _ -> Just x


