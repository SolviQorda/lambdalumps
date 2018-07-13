module IO.LeftRight where

import Model.Tetronimo

tetronimoLeft :: Tetronimo -> SettledBlocks -> Tetronimo
tetronimoLeft tet blocks
  | (xcoord $ first tet)  == 0    = tet
  | (xcoord $ second tet) == 0    = tet
  | (xcoord $ third tet)  == 0    = tet
  | (xcoord $ fourth tet) == 0    = tet
  | (Pos (pred $ xcoord $ first tet)(ycoord $ first tet)) `elem` blocks      = tet
  | (Pos (pred $ xcoord $ second tet)(ycoord $ second tet)) `elem` blocks     = tet
  | (Pos (pred $ xcoord $ third tet)(ycoord $ third tet)) `elem` blocks      = tet
  | (Pos (pred $ xcoord $ fourth tet)(ycoord $ fourth tet)) `elem` blocks    = tet
  | otherwise = Tetronimo
      (Pos (pred (xcoord $ first tet)) (ycoord $ first tet))
      (Pos (pred (xcoord $ second tet)) (ycoord $ second tet))
      (Pos (pred (xcoord $ third tet)) (ycoord $ third tet))
      (Pos (pred (xcoord $ fourth tet)) (ycoord $ fourth tet))
      (shape tet)
      (rotation tet)

tetronimoRight :: Tetronimo -> SettledBlocks -> Tetronimo
tetronimoRight tet blocks
  | (xcoord $ first tet)  == 9    = tet
  | (xcoord $ second tet) == 9    = tet
  | (xcoord $ third tet)  == 9    = tet
  | (xcoord $ fourth tet) == 9    = tet
  | (Pos (succ $ xcoord $ first tet)(ycoord $ first tet)) `elem` blocks   = tet
  | (Pos (succ $ xcoord $ second tet)(ycoord $ second tet)) `elem` blocks  = tet
  | (Pos (succ $ xcoord $ third tet)(ycoord $ third tet)) `elem` blocks   = tet
  | (Pos (succ $ xcoord $ fourth tet)(ycoord $ fourth tet)) `elem` blocks   = tet
  | otherwise = Tetronimo
      (Pos (succ (xcoord $ first tet)) (ycoord $ first tet))
      (Pos (succ (xcoord $ second tet)) (ycoord $ second tet))
      (Pos (succ (xcoord $ third tet)) (ycoord $ third tet))
      (Pos (succ (xcoord $ fourth tet)) (ycoord $ fourth tet))
      (shape tet)
      (rotation tet)
