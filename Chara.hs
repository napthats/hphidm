module Chara
       (
         Chara,
         PlayerChara(),
         walk,
         makePlayerChara,
         getAroundCharaView,
        ) where

import qualified PhiMap as PM
import Data.List (elemIndex, findIndex, find)


class Chara a where
  walk :: PM.PhiMap -> PM.Direction -> a -> a
  walk phi_map dir chara =
    let neighbor_pos = getNeighborPosition phi_map dir chara in
    if canEnterPosition phi_map neighbor_pos chara
    then changePosition neighbor_pos chara else chara
  
  getNeighborPosition :: PM.PhiMap -> PM.Direction -> a -> PM.Position
  getNeighborPosition phi_map (PM.AbsoluteDirection adir) chara =
    PM.getNextPosition phi_map (getPosition chara) adir
  getNeighborPosition phi_map (PM.RelativeDirection rdir) chara =
    let adir = getDirection chara in
    PM.getNextPosition phi_map (getPosition chara) (PM.turnAbsoluteDirection adir rdir)
  
  canEnterPosition :: PM.PhiMap -> PM.Position -> a -> Bool
  changePosition :: PM.Position -> a -> a
  getPosition :: a -> PM.Position
  getDirection :: a -> PM.AbsoluteDirection
  getCharaView :: PM.AbsoluteDirection -> (Int, Int, a) -> CharaView
  getName :: a -> String


data PlayerChara = PlayerChara {
  pcPosition :: PM.Position,
  pcDirection :: PM.AbsoluteDirection,
  pcName :: String} deriving (Show)

instance Chara PlayerChara where
  canEnterPosition phi_map pos _ = PM.isNormalEnterable (PM.getPhiMapChip phi_map pos)
  changePosition pos chara = PlayerChara {pcPosition = pos, pcDirection = pcDirection chara, pcName = pcName chara}
  getPosition chara = pcPosition chara
  getDirection chara = pcDirection chara
  getName chara = pcName chara
  getCharaView dir (x, y, chara) = CharaView
    {viewX = x, viewY = y,
     viewDirection = PM.calculateRelativeDirection dir $ getDirection chara, viewName = getName chara}

makePlayerChara :: PM.PhiMap -> PlayerChara
makePlayerChara phi_map = PlayerChara {
  pcPosition = PM.getDefaultPosition phi_map,
  pcDirection = PM.North,
  pcName = "test"}

getAroundCharaView :: (Chara a) => PM.AbsoluteDirection -> [[PM.Position]] -> [a] -> [CharaView]
getAroundCharaView dir pos_list chara_list =
  let pos_chara_list =
        map (\chara -> getSucceedOrd (map (elemIndex (getPosition chara)) pos_list) chara) chara_list
  in map (getCharaView dir) $ 
     map (\x -> case x of Nothing -> undefined; Just y -> y) $
     filter (\x -> case x of Nothing -> False; Just _ -> True) pos_chara_list
  where
    getSucceedOrd list chara =
      let index = findIndex (\x -> case x of Nothing -> False; Just _ -> True) list
      in let succeed = find (\x -> case x of Nothing -> False; Just _ -> True) list
      in case succeed of
              Nothing -> Nothing
              Just maybe_xpos -> case maybe_xpos of
                Nothing -> Nothing
                Just xpos -> case index of
                  Nothing -> Nothing
                  Just ypos -> Just (xpos, ypos, chara)

data CharaView = CharaView {viewX :: Int, viewY :: Int, viewDirection :: PM.RelativeDirection, viewName :: String} deriving (Show)
