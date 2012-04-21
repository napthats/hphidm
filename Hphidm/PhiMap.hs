module Hphidm.PhiMap
       (
         Direction(..),
         Position(),
         getPosWithDir,
         canEnter,
       ) where



data Direction = North | East | West | South
newtype Position = Position (Int, Int)

getPosWithDir :: Position -> Direction -> IO Position
getPosWithDir (Position (x, y)) North = return $ Position (x, y-1)
getPosWithDir (Position (x, y)) East = return $ Position (x+1, y)
getPosWithDir (Position (x, y)) West = return $ Position (x-1, y)
getPosWithDir (Position (x, y)) South = return $ Position (x, y+1)

canEnter :: Position -> IO Bool
canEnter (Position (x, y)) = return $ x >= 0 && y >= 0

