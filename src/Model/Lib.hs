module Model.Lib where

--LambdaLumps
import Model.Score
import Model.Tetronimo

import Data.List
import System.Random

--is the game over? Tetronimo needs to be settled
gameOver :: Tetronimo -> Bool
gameOver setTet
  | topYCoord setTet > screenRoof = True
  | otherwise                     = False

--what rows need to be cleared - built from the SettledBlocks dataype.
--if length of filtered list of yPos's with ycoord = x is 10, then get rid of that sublist, look at the next sublist. 18 is the top row.

--TODO:make clear output a tuple of (SettledBlocks, Int) of how many rows were cleared.
clear :: SettledBlocks -> (SettledBlocks, Int)
clear bs = go 0 bs 0
  where
    go z [] count = ([], count)
    go z (b:bs) count
      | z == (succ screenRoof)         = ((b:bs), count)
      | length ys == 10 = go (succ z) ((b:bs) \\ ys) (succ count)
      | otherwise       = go (succ z) (b:bs) count
        where ys = filter (\x -> ycoord x == z) (b:bs)


--Placeholder :: TODO make this functional
level :: Int
level = 3

--Using a case syntax for now until i can improve the algorithm.
-- is the pos == a block with the same x and a y that is one less
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

--hard set for now.

screenRoof :: Int
screenRoof = 19

topYCoord :: Tetronimo -> Int
topYCoord tet = maximum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

lowYCoord :: Tetronimo -> Int
lowYCoord tet = minimum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

--just a one tetronimo fall
move :: Tetronimo -> Tetronimo
move tet = Tetronimo
            (Pos (xcoord $ first tet) (pred (ycoord $ first tet)))
            (Pos (xcoord $ second tet) (pred (ycoord $ second tet)))
            (Pos (xcoord $ third tet) (pred (ycoord $ third tet)))
            (Pos (xcoord $ fourth tet) (pred (ycoord $ fourth tet)))
            (shape tet)
            (rotation tet)

--just a one cell fall
moveBlock :: Pos -> Pos
moveBlock bloc = Pos (xcoord bloc) (pred $ ycoord bloc)

--need to make sure I can handle multiple lines falling down.
--helper function for clear - moves all settledBlocks down by one.
collapseBlocks :: SettledBlocks -> SettledBlocks
collapseBlocks xs = go 0 xs
  where
    go z xs
    --for x in xs // or fo rz
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
--need to have a means of making a new list with the transfomred qs, and the remaining xs, minus the old qs.

settleTetronimo :: Tetronimo -> SettledBlocks
settleTetronimo tet = [first tet, second tet, third tet, fourth tet]

--
-- | Types
