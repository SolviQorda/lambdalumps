module Model.RandomTetronimo where

import System.IO.Unsafe

import Model.Tetronimo

getTetronimo :: Int -> Tetronimo
getTetronimo q
  | q > (l - 1) = spawn (sequenceList !! (q `mod` l))
  | otherwise   = spawn (sequenceList !! q)
    where l = length $ sequenceList

spawn :: Int -> Tetronimo
spawn x
  -- s tetronimo
  | x == 1    = Tetronimo (Pos 4 20) (Pos 5 20) (Pos 5 21) (Pos 6 21) SShape Zero
  -- z tetronimo
  | x == 2    = Tetronimo (Pos 4 20) (Pos 5 20) (Pos 3 21) (Pos 4 21) ZShape Zero
  -- t tetronimo
  | x == 3    = Tetronimo (Pos 3 20) (Pos 4 20) (Pos 5 20) (Pos 4 21) TShape Zero
 -- i tetronimo
  | x == 4    = Tetronimo (Pos 3 20) (Pos 4 20) (Pos 5 20) (Pos 6 20) IShape Zero
  -- o tetronimo
  | x == 5    = Tetronimo (Pos 4 20) (Pos 5 20) (Pos 4 21) (Pos 5 21) OShape Zero
  -- j tetronimo
  | x == 6    = Tetronimo (Pos 4 20) (Pos 5 20) (Pos 6 20) (Pos 4 21) JShape Zero
  -- l tetronimo
  | x == 7    = Tetronimo (Pos 3 20) (Pos 4 20) (Pos 5 20) (Pos 5 21) LShape Zero
  --error
  | otherwise = Tetronimo (Pos 4 20) (Pos 4 20) (Pos 4 20) (Pos 4 20) IShape Zero

spawnCode :: Shape -> Int
spawnCode shape
  | shape == SShape = 1
  | shape == ZShape = 2
  | shape == TShape = 3
  | shape == IShape = 4
  | shape == OShape = 5
  | shape == JShape = 6
  | shape == LShape = 7
  | otherwise       = 1

sequenceList :: [Int]
sequenceList = [6, 2,	3, 6,	1,
                1, 1,	5, 2,	1,
                7, 4,	7, 2,	3,
                2, 6,	1, 2,	4,
                5, 5,	4, 6,	3,
                5, 4,	1, 7,	3,
                5, 4,	7, 5,	6,
                3, 2,	3, 4,	6]
