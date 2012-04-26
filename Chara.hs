module Chara
       (
         Chara,
         PlayerChara(),
         walk,
         makePlayerChara,
        ) where

import qualified PhiMap as PM


class Chara a where
  walk :: PM.PhiMap -> PM.Direction -> a -> a
  walk phi_map dir chara =
    let maybe_neighbor_pos = getNeighborPosition phi_map dir chara in
    case maybe_neighbor_pos of
      Nothing -> chara
      Just neighbor_pos ->
        if canEnterPosition phi_map neighbor_pos chara
           then changePosition neighbor_pos chara else chara
  
  getNeighborPosition :: PM.PhiMap -> PM.Direction -> a -> Maybe PM.Position
  getNeighborPosition phi_map (PM.AbsoluteDirection adir) chara =
    PM.getNextPosition phi_map (getPosition chara) adir
  getNeighborPosition phi_map (PM.RelativeDirection rdir) chara =
    let adir = getDirection chara in
    PM.getNextPosition phi_map (getPosition chara) (PM.turnAbsoluteDirection adir rdir)
  
  canEnterPosition :: PM.PhiMap -> PM.Position -> a -> Bool
  changePosition :: PM.Position -> a -> a
  getPosition :: a -> PM.Position
  getDirection :: a -> PM.AbsoluteDirection


data PlayerChara = PlayerChara {
  position :: PM.Position,
  direction :: PM.AbsoluteDirection,
  name :: String} deriving (Show)

instance Chara PlayerChara where
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
  changePosition pos chara = PlayerChara {position = pos, direction = direction chara, name = name chara}
  getPosition chara = position chara
  getDirection chara = direction chara

makePlayerChara :: PM.PhiMap -> PlayerChara
makePlayerChara phi_map = PlayerChara {
  position = PM.getDefaultPosition phi_map,
  direction = PM.North,
  name = "test"}
