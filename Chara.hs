module Chara
       (
         Chara(..),
         CharaView(..),
         getAroundCharaView,
        ) where

import qualified PhiMap as PM
import Data.List (elemIndex, findIndex, find)


data CharaView = CharaView {viewX :: Int, viewY :: Int, viewDirection :: PM.RelativeDirection, viewName :: String} deriving (Show)

class Chara a where
  walk :: PM.PhiMap -> PM.Direction -> a -> Maybe a
  walk phi_map dir chara =
    let neighbor_pos = getNeighborPosition phi_map dir chara in
    if canEnterPosition phi_map neighbor_pos chara
    then Just (changePosition neighbor_pos chara) else Nothing
  
  turn :: PM.Direction -> a -> a
  turn dir chara =
    let next_dir = case dir of
          PM.AbsoluteDirection adir -> adir
          PM.RelativeDirection rdir -> PM.turnAbsoluteDirection (getDirection chara) rdir
    in changeDirection next_dir chara

  getNeighborPosition :: PM.PhiMap -> PM.Direction -> a -> PM.Position
  getNeighborPosition phi_map (PM.AbsoluteDirection adir) chara =
    PM.getNextPosition phi_map (getPosition chara) adir
  getNeighborPosition phi_map (PM.RelativeDirection rdir) chara =
    let adir = getDirection chara in
    PM.getNextPosition phi_map (getPosition chara) (PM.turnAbsoluteDirection adir rdir)
  
  canEnterPosition :: PM.PhiMap -> PM.Position -> a -> Bool
  changePosition :: PM.Position -> a -> a
  changeDirection :: PM.AbsoluteDirection -> a -> a
  getPosition :: a -> PM.Position
  getDirection :: a -> PM.AbsoluteDirection
  getCharaView :: PM.AbsoluteDirection -> (Int, Int, a) -> CharaView
  getName :: a -> String


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
