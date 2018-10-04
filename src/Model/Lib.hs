module Model.Lib where

--LambdaLumps
import Model.Score
import Model.Tetronimo

import Data.List

--is the game over? Tetronimo needs to be settled
gameOver :: Tetronimo -> Bool
gameOver setTet
  | topYCoord setTet > screenRoof = True
  | otherwise                     = False

--clear blocks on full lines
clear :: SettledBlocks -> (SettledBlocks, Int)
clear bs = go 0 bs 0
  where
    go z [] count = ([], count)
    go z (b:bs) count
      | z == (succ screenRoof)         = ((b:bs), count)
      | length ys == 10 = go (succ z) ((b:bs) \\ ys) (succ count)
      | otherwise       = go (succ z) (b:bs) count
        where ys = filter (\x -> ycoord x == z) (b:bs)

isItSettled :: Tetronimo -> SettledBlocks -> Bool
isItSettled tet set
  | (ycoord $ first tet)  == 0                                         = True
  | (ycoord $ third tet)  == 0                                         = True
  | (ycoord $ second tet) == 0                                         = True
  | (ycoord $ fourth tet) == 0                                         = True
  | Pos (xcoord $ first tet)(pred $ ycoord $ first tet) `elem` set     = True
  | Pos (xcoord $ second tet)(pred $ ycoord $ second tet) `elem` set   = True
  | Pos (xcoord $ third tet)(pred $ ycoord $ third tet) `elem` set     = True
  | Pos (xcoord $ fourth tet)(pred $ ycoord $ fourth tet) `elem` set   = True
  | otherwise                                                          = False

screenRoof :: Int
screenRoof = 19

topYCoord :: Tetronimo -> Int
topYCoord tet = maximum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

lowYCoord :: Tetronimo -> Int
lowYCoord tet = minimum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

--current tetronimo falls by one place
move :: Tetronimo -> Tetronimo
move tet = Tetronimo
            (Pos (xcoord $ first tet) (pred (ycoord $ first tet)))
            (Pos (xcoord $ second tet) (pred (ycoord $ second tet)))
            (Pos (xcoord $ third tet) (pred (ycoord $ third tet)))
            (Pos (xcoord $ fourth tet) (pred (ycoord $ fourth tet)))
            (shape tet)
            (rotation tet)

--move a cell by one place
moveBlock :: Pos -> Pos
moveBlock bloc = Pos (xcoord bloc) (pred $ ycoord bloc)

--helper function for clear - moves all settledBlocks down by one.
collapseBlocks :: SettledBlocks -> SettledBlocks
collapseBlocks xs = go 0 xs
  where
    go z xs
    --for x in xs // or for z
    --are there blocks on row z? is there an empty row below x?
      | z == (succ screenRoof)                          = xs
      --if the current row has no blocks, then there's nothing to fall
      | (length $ qs) == 0                             = go (succ z) xs
      --if there are blocks in this row and none below, move all blocks down
      | (z > 0) && ((length $ qs) > 0) && (length ys == 0)        = go (pred z) (((xs \\ qs) ++  zs))
      | otherwise                                      = go (succ z) xs
      where qs = filter (\x -> (ycoord x) == z) xs
            ys = filter (\x -> (succ $ ycoord x) == z) xs
            zs = map (moveBlock) qs

settleTetronimo :: Tetronimo -> SettledBlocks
settleTetronimo tet = [first tet, second tet, third tet, fourth tet]

isInside :: Tetronimo -> SettledBlocks -> Bool
isInside tet blocks = any (\pos -> pos `elem` blocks) [first tet, second tet, third tet, fourth tet]
