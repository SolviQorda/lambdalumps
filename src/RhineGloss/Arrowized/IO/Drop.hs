module RhineGloss.Arrowized.IO.Drop where

import Data.List

--LambdaLumps
import RhineGloss.Arrowized.Model.Lib
import RhineGloss.Arrowized.Model.Tetronimo

dropTet :: Tetronimo -> SettledBlocks -> Tetronimo
dropTet tet blocks =
        Tetronimo
            (Pos (xcoord $ first tet) ((ycoord $ first tet) - difference))
            (Pos (xcoord $ second tet) ((ycoord $ second tet) - difference))
            (Pos (xcoord $ third tet) ((ycoord $ third tet) - difference))
            (Pos (xcoord $ fourth tet) ((ycoord $ fourth tet) - difference))
            (shape tet)
            (rotation tet)
              where difference = (lowYCoord tet) - (ycoord $ (highestObstruction tet blocks)) -1

highestObstruction :: Tetronimo -> SettledBlocks -> Pos
highestObstruction tet blocks =
  go (tetColumns tet) blocks []
    where
      go qs [] _ = Pos 0 0
      go [] _ xs =  maximum $ xs
      go (q:qs) blocks xs = go qs blocks ((highestInColumn blocks q):xs)

--finds the highest settled block in the column
highestInColumn :: SettledBlocks -> Int -> Pos
highestInColumn blocks q
  | blocks       == [] = Pos 0 0
  | obstructions == [] = Pos 0 0
  | otherwise          = maximum $ obstructions
      where obstructions = filter (\x -> (xcoord x) == q) blocks

--Columns occupied by the tetronimo
tetColumns :: Tetronimo -> [Int]
tetColumns tet = nub $ [(xcoord $ first tet), (xcoord $ second tet), (xcoord $ third tet), (xcoord $ fourth tet)]
