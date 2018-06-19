
module IO.Rotate where

import Lib

--Rotation Functions

rotateCW :: Tetronimo -> Tetronimo
rotateCW tet
    | s == OShape = tet
    | s == IShape = rotateICW tet
    | s == SShape = rotateSCW tet
    | s == ZShape = rotateZCW tet
    | s == LShape = rotateLCW tet
    | s == JShape = rotateJCW tet
    | s == TShape = rotateTCW tet
    | otherwise = tet
        where s = shape tet

rotateICW :: Tetronimo -> Tetronimo
rotateICW tet
  | rot == Zero =
  Tetronimo
    (Pos (succ        (xcoord $ first tet)) (succ        (ycoord $ first tet)))
    (Pos              (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (pred (xcoord $ third tet))        (pred        (ycoord $ third tet)))
    (Pos (pred $ pred (xcoord $ fourth tet)) (pred $ pred (ycoord $ fourth tet)))
    (shape tet)
    Ninety
  | rot == Ninety =
  Tetronimo
    (Pos (pred        (xcoord $ first tet)) (pred        (ycoord $ first tet)))
    (Pos              (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (succ (xcoord $ third tet))        (succ        (ycoord $ third tet)))
    (Pos (succ $ succ (xcoord $ fourth tet)) (succ $ succ (ycoord $ fourth tet)))
    (shape tet)
    OneEighty
  | rot == OneEighty =
  Tetronimo
    (Pos (succ        (xcoord $ first tet)) (succ        (ycoord $ first tet)))
    (Pos              (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (pred (xcoord $ third tet))        (pred        (ycoord $ third tet)))
    (Pos (pred $ pred (xcoord $ fourth tet)) (pred $ pred (ycoord $ fourth tet)))
    (shape tet)
    TwoSeventy
  | rot == TwoSeventy =
  Tetronimo
    (Pos (pred        (xcoord $ first tet)) (pred        (ycoord $ first tet)))
    (Pos              (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (succ (xcoord $ third tet))        (succ        (ycoord $ third tet)))
    (Pos (succ $ succ (xcoord $ fourth tet)) (succ $ succ (ycoord $ fourth tet)))
    (shape tet)
    Zero
        where rot = (rotation tet)

rotateSCW :: Tetronimo -> Tetronimo
rotateSCW tet
  | rot == Zero =
  Tetronimo
    (Pos (succ (xcoord $ first tet)) (succ        (ycoord $ first tet)))
    (Pos       (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (succ (xcoord $ third tet)) (pred        (ycoord $ third tet)))
    (Pos       (xcoord $ fourth tet)  (pred $ pred (ycoord $ fourth tet)))
    (shape tet)
    Ninety
  | rot == Ninety =
  Tetronimo
    (Pos (pred (xcoord $ first tet)) (pred        (ycoord $ first tet)))
    (Pos       (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (pred (xcoord $ third tet)) (succ        (ycoord $ third tet)))
    (Pos       (xcoord $ fourth tet)  (succ $ succ (ycoord $ fourth tet)))
    (shape tet)
    OneEighty
  | rot == OneEighty =
  Tetronimo
    (Pos (succ (xcoord $ first tet)) (succ        (ycoord $ first tet)))
    (Pos       (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (succ (xcoord $ third tet)) (pred        (ycoord $ third tet)))
    (Pos       (xcoord $ fourth tet)  (pred $ pred (ycoord $ fourth tet)))
    (shape tet)
    TwoSeventy
  | otherwise =
  Tetronimo
    (Pos (pred (xcoord $ first tet)) (pred        (ycoord $ first tet)))
    (Pos       (xcoord $ second tet)               (ycoord $ second tet))
    (Pos (pred (xcoord $ third tet)) (succ        (ycoord $ third tet)))
    (Pos       (xcoord $ fourth tet)  (succ $ succ (ycoord $ fourth tet)))
    (shape tet)
    Zero
        where rot = (rotation tet)

--  | x == 2    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 3 20) (Pos 4 20) ZShape Zero

rotateZCW :: Tetronimo -> Tetronimo
rotateZCW tet
  | rot == Zero =
  Tetronimo
    (Pos              (xcoord $ first tet)                (ycoord $ first tet))
    (Pos (pred        (xcoord $ second tet))  (pred        (ycoord $ second tet)))
    (Pos (succ $ succ (xcoord $ third tet))               (ycoord $ third tet))
    (Pos              (xcoord $ fourth tet)   (pred $ pred (ycoord $ fourth tet)))
    (shape tet)
    Ninety
  | rot == Ninety =
  Tetronimo
    (Pos              (xcoord $ first tet)                (ycoord $ first tet))
    (Pos (succ        (xcoord $ second tet))  (succ        (ycoord $ second tet)))
    (Pos (pred $ pred (xcoord $ third tet))               (ycoord $ third tet))
    (Pos (            (xcoord $ fourth tet))  (succ $ succ (ycoord $ fourth tet)))
    (shape tet)
    OneEighty
  | rot == OneEighty =
  Tetronimo
    (Pos              (xcoord $ first tet)                (ycoord $ first tet))
    (Pos (pred        (xcoord $ second tet))  (pred        (ycoord $ second tet)))
    (Pos (succ $ succ (xcoord $ third tet))               (ycoord $ third tet))
    (Pos              (xcoord $ fourth tet)   (pred $ pred (ycoord $ fourth tet)))
    (shape tet)
    TwoSeventy
  | otherwise =
  Tetronimo
    (Pos              (xcoord $ first tet)                (ycoord $ first tet))
    (Pos (succ        (xcoord $ second tet))  (succ        (ycoord $ second tet)))
    (Pos (pred $ pred (xcoord $ third tet))               (ycoord $ third tet))
    (Pos (            (xcoord $ fourth tet))  (succ $ succ (ycoord $ fourth tet)))
    (shape tet)
    Zero
        where rot = (rotation tet)

rotateTCW :: Tetronimo -> Tetronimo
rotateTCW tet
  | rot == Zero =
  Tetronimo
    (Pos (succ x1) (succ y1))
    (Pos       x2        y2)
    (Pos (pred x3) (pred y3))
    (Pos (succ x4) (pred y4))
    (shape tet)
    Ninety
  | rot == Ninety =
  Tetronimo
    (Pos (succ x1) (pred y1))
    (Pos       x2        y2)
    (Pos (pred x3) (succ y3))
    (Pos (pred x4) (pred y4))
    (shape tet)
    OneEighty
  | rot == OneEighty =
  Tetronimo
    (Pos (pred x1) (pred y1))
    (Pos       x2        y2)
    (Pos (succ x3) (succ y3))
    (Pos (pred x4) (succ y4))
    (shape tet)
    TwoSeventy
  | otherwise =
  Tetronimo
    (Pos (pred x1) (pred y1))
    (Pos       x2        y2)
    (Pos (succ x3) (pred y3))
    (Pos (succ x4) (succ y4))
    (shape tet)
    Zero
      where rot = (rotation tet)
            x1 = (xcoord $ first tet)
            x2 = (xcoord $ second tet)
            x3 = (xcoord $ third tet)
            x4 = (xcoord $ fourth tet)
            y1 = (ycoord $ first tet)
            y2 = (ycoord $ second tet)
            y3 = (ycoord $ third tet)
            y4 = (ycoord $ fourth tet)

rotateLCW :: Tetronimo -> Tetronimo
rotateLCW tet
  | rot == Zero =
  Tetronimo
    (Pos (succ        x1) (succ        y1))
    (Pos              x2               y2)
    (Pos (pred        x3) (pred        y3))
    (Pos              x4  (pred $ pred y4))
    (shape tet)
    Ninety
  | rot == Ninety =
  Tetronimo
    (Pos (succ        x1) (pred        y1))
    (Pos              x2               y2)
    (Pos (pred        x3) (succ        y3))
    (Pos (pred $ pred x4)              y4)
    (shape tet)
    OneEighty
  | rot == OneEighty =
  Tetronimo
    (Pos (pred        x1) (pred        y1))
    (Pos              x2               y2)
    (Pos (succ        x3) (succ        y3))
    (Pos              x4  (succ $ succ y4))
    (shape tet)
    TwoSeventy
  | rot == TwoSeventy =
  Tetronimo
    (Pos (pred        x1) (succ        y1))
    (Pos              x2               y2)
    (Pos (succ        x3) (pred        y3))
    (Pos (succ $ succ x4)              y4)
    (shape tet)
    Zero
  | otherwise = tet
      where rot = (rotation tet)
            x1 = (xcoord $ first tet)
            x2 = (xcoord $ second tet)
            x3 = (xcoord $ third tet)
            x4 = (xcoord $ fourth tet)
            y1 = (ycoord $ first tet)
            y2 = (ycoord $ second tet)
            y3 = (ycoord $ third tet)
            y4 = (ycoord $ fourth tet)

rotateJCW :: Tetronimo -> Tetronimo
rotateJCW tet
  | rot == Zero =
  Tetronimo
    (Pos (succ        x1) (succ        y1))
    (Pos              x2               y2)
    (Pos (pred        x3) (pred        y3))
    (Pos (succ $ succ x4)              y4)
    (shape tet)
    Ninety
  | rot == Ninety =
  Tetronimo
    (Pos (succ        x1) (pred        y1))
    (Pos              x2               y2)
    (Pos (pred        x3) (succ        y3))
    (Pos              x4  (pred $ pred y4))
    (shape tet)
    OneEighty
  | rot == OneEighty =
  Tetronimo
    (Pos (pred        x1) (pred        y1))
    (Pos              x2               y2)
    (Pos (succ        x3) (succ        y3))
    (Pos (pred $ pred x4)              y4)
    (shape tet)
    TwoSeventy
  | otherwise =
  Tetronimo
    (Pos (pred        x1) (succ        y1))
    (Pos              x2               y2)
    (Pos (succ        x3) (pred        y3))
    (Pos              x4  (succ $ succ y4))
    (shape tet)
    Zero
      where rot = (rotation tet)
            x1 = (xcoord $ first tet)
            x2 = (xcoord $ second tet)
            x3 = (xcoord $ third tet)
            x4 = (xcoord $ fourth tet)
            y1 = (ycoord $ first tet)
            y2 = (ycoord $ second tet)
            y3 = (ycoord $ third tet)
            y4 = (ycoord $ fourth tet)
