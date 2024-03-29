module Chara
       (
         Chara(..),
         CharaView(..),
         InjuredBy(..),
         getCharaInRegion,
        ) where

import Data.List (elemIndex, findIndex, find)
import qualified PhiMap as PM
import qualified Combat as CO
import qualified Item as IT
import CharaData


data CharaView = CharaView Int Int PM.RelativeDirection String deriving (Show)

class Chara a where
  walk :: PM.PhiMap -> PM.Direction -> a -> Maybe a
  walk phi_map dir chara =
    let neighbor_pos = getNeighborPosition phi_map dir chara in
    if canEnterPosition phi_map neighbor_pos chara
    then Just (changePosition neighbor_pos chara) else Nothing
  
  turn :: PM.Direction -> a -> Maybe a
  turn dir chara =
    let next_dir = case dir of
          PM.AbsoluteDirection adir -> adir
          PM.RelativeDirection rdir -> PM.turnAbsoluteDirection (getDirection chara) rdir
    in Just $ changeDirection next_dir chara

  getNeighborPosition :: PM.PhiMap -> PM.Direction -> a -> PM.Position
  getNeighborPosition phi_map (PM.AbsoluteDirection adir) chara =
    PM.getNextPosition phi_map (getPosition chara) adir
  getNeighborPosition phi_map (PM.RelativeDirection rdir) chara =
    let adir = getDirection chara in
    PM.getNextPosition phi_map (getPosition chara) (PM.turnAbsoluteDirection adir rdir)
  
  canSee :: PM.PhiMap -> PM.Position -> a -> Bool
  canSee phimap pos chara = any (== pos) $ concat $ getSight phimap chara
  
  changePosition :: PM.Position -> a -> a
  changeDirection :: PM.AbsoluteDirection -> a -> a
  addHp :: Int -> InjuredBy -> a -> a
  canEnterPosition :: PM.PhiMap -> PM.Position -> a -> Bool
  getPosition :: a -> PM.Position
  getDirection :: a -> PM.AbsoluteDirection
  getCharaView :: PM.AbsoluteDirection -> (Int, Int, a) -> CharaView
  getName :: a -> String
  getMhp :: a -> Int
  getHp :: a -> Int
  getMmp :: a -> Int
  getMp :: a -> Int
  isDead :: a -> Bool
  getSight :: PM.PhiMap -> a -> [[PM.Position]]
  hitTo :: Chara b => a -> b -> (a, b, CO.CombatResult)
  getHitRange :: PM.PhiMap -> a -> [PM.Position]
  getLastInjured :: a -> Maybe InjuredBy
  addItem :: IT.Item -> a -> a
  deleteItem :: Int -> a -> Maybe (a, IT.Item)
  getItemList :: a -> [IT.Item]

getCharaInRegion :: (Chara a) => [[PM.Position]] -> [a] -> [(Int, Int, a)]
getCharaInRegion pos_list chara_list =
  let pos_chara_list =
        map (\chara -> getSucceedOrd (map (elemIndex (getPosition chara)) pos_list) chara) chara_list
  in map (\x -> case x of Nothing -> error "Assertion error: getCharaInRegion"; Just y -> y) $
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
