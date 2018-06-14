module Lib where

import Data.List
import System.Random

--is the game over? Tetronimo needs to be settled
gameOver :: Tetronimo -> Bool
gameOver setTet
  | topYCoord setTet > screenRoof = True
  | otherwise                     = False

--what rows need to be cleared - built from the SettledBlocks dataype.
--if length of filtered list of yPos's with ycoord = x is 10, then get rid of that sublist, look at the next sublist. 18 is the top row.
clear :: SettledBlocks -> SettledBlocks
clear bs = go 0 bs
  where
    go z [] = []
    go z (b:bs)
      | z == (succ screenRoof)         = (b:bs)
      | length ys == 10 = go (succ z) ((b:bs) \\ ys)
      | otherwise       = go (succ z) (b:bs)
        where ys = filter (\x -> ycoord x == z) (b:bs)


--TODO: a version of clear leveraging the playfield datatype, which inherits lists of settledBlocks, and then does a lookup on each row to see if it `elem` Empty -> if it doesn't then it clears.

-- | Given that this is the next next tetronimo, output the gamestate that arises from that
settle :: Tetronimo -> Gamestate -> Gamestate
settle nxnxtet g
  | isItSettled tet blocks = Gamestate nxnxtet nxtet (collapseBlocks $ clear $  (settleTetronimo tet)++ blocks)
  | otherwise              = Gamestate (move tet) nxtet blocks
    where
      tet    = currentTetronimo g
      nxtet  = nextTetronimo g
      blocks = settledTetronimos g
-- some gameover check.

--Using a case syntax for now until i can improve the algorithm. TODO: Decide whether to use a sorting function
-- is the pos == a block with the same x and a y that is one less
isItSettled :: Tetronimo -> SettledBlocks -> Bool
isItSettled tet set
  | (ycoord $ first tet) == 0                                          = True
  | (ycoord $ third tet) == 0                                          = True
  | (ycoord $ second tet) == 0                                         = True
  | (ycoord $ fourth tet) == 0                                         = True
  | Pos (xcoord $ first tet)(pred $ ycoord $ first tet) `elem` set   = True
  | Pos (xcoord $ second tet)(pred $ ycoord $ second tet) `elem` set = True
  | Pos (xcoord $ third tet)(pred $ ycoord $ third tet) `elem` set   = True
  | Pos (xcoord $ fourth tet)(pred $ ycoord $ fourth tet) `elem` set = True
  | otherwise                                                        = False

--Int until I think of a better way - assuming an RNG
spawn :: Int -> Tetronimo
spawn x
  -- s tetronimo
  | x == 1    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 5 20) (Pos 6 20)
  -- z tetronimo
  | x == 2    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 3 20) (Pos 4 20)
  -- t tetronimo
  | x == 3    = Tetronimo (Pos 3 19) (Pos 4 19) (Pos 5 19) (Pos 4 20)
  -- i tetronimo
  | x == 4    = Tetronimo (Pos 3 19) (Pos 4 19) (Pos 5 19) (Pos 6 19)
  -- o tetronimo
  | x == 5    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 4 20) (Pos 5 20)
  -- j tetronimo
  | x == 6    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 6 19) (Pos 4 20)
  -- i tetronimo
  | x == 7    = Tetronimo (Pos 3 19) (Pos 4 19) (Pos 5 19) (Pos 5 20)
  --error
  | otherwise = Tetronimo (Pos 4 19) (Pos 4 19) (Pos 4 19) (Pos 4 19)

-- getRandom :: IO Int
-- getRandom = do
--   r1 <- getStdGen
--   let (x, r2) = randomR (1, 7) r1
--   setStdGen r2
--   return x

--hard set for now.

screenRoof :: Int
screenRoof = 18

topYCoord :: Tetronimo -> Int
topYCoord tet = maximum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

lowYCoord :: Tetronimo -> Int
lowYCoord tet = minimum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

--just a one cell fall for now TODO: handle input, direction, rotation.
move :: Tetronimo -> Tetronimo
move tet = Tetronimo
            (Pos (xcoord $ first tet) (pred (ycoord $ first tet)))
            (Pos (xcoord $ second tet) (pred (ycoord $ second tet)))
            (Pos (xcoord $ third tet) (pred (ycoord $ third tet)))
            (Pos (xcoord $ fourth tet) (pred (ycoord $ fourth tet)))

--helper function for clear - moves all settledBlocks down by one.
collapseBlocks :: SettledBlocks -> SettledBlocks
collapseBlocks xs = go xs []
  where
    go [] qs = qs
    go (x:xs) qs
      --this shouldnt be necessary, but exists as a safeguard against blocks dropping off the edge of the playfield
      | ycoord x == 0 = go xs (x:qs)
      | otherwise     = go xs (q:qs)
        where q = Pos (xcoord x) (pred $ ycoord x)

settleTetronimo :: Tetronimo -> SettledBlocks
settleTetronimo tet = [first tet, second tet, third tet, fourth tet]

tetronimoLeft :: Tetronimo -> Tetronimo
tetronimoLeft tet
  | (xcoord $ first tet)  == 0    = tet
  | (xcoord $ second tet) == 0    = tet
  | (xcoord $ third tet)  == 0    = tet
  | (xcoord $ fourth tet) == 0    = tet
  | otherwise = Tetronimo
      (Pos (pred (xcoord $ first tet)) (ycoord $ first tet))
      (Pos (pred (xcoord $ second tet)) (ycoord $ second tet))
      (Pos (pred (xcoord $ third tet)) (ycoord $ third tet))
      (Pos (pred (xcoord $ fourth tet)) (ycoord $ fourth tet))

tetronimoRight :: Tetronimo -> Tetronimo
tetronimoRight tet
  | (xcoord $ first tet)  == 9    = tet
  | (xcoord $ second tet) == 9    = tet
  | (xcoord $ third tet)  == 9    = tet
  | (xcoord $ fourth tet) == 9    = tet
  | otherwise = Tetronimo
      (Pos (succ (xcoord $ first tet)) (ycoord $ first tet))
      (Pos (succ (xcoord $ second tet)) (ycoord $ second tet))
      (Pos (succ (xcoord $ third tet)) (ycoord $ third tet))
      (Pos (succ (xcoord $ fourth tet)) (ycoord $ fourth tet))


-- | Types

data Gamestate =
  Gamestate {
    nextTetronimo     :: Tetronimo,
    currentTetronimo  :: Tetronimo,
    settledTetronimos :: SettledBlocks
}

data Playfield =
  Playfield {
    row0 :: Row,
    row1 :: Row,
    row2 :: Row,
    row3 :: Row,
    row4 :: Row,
    row5 :: Row,
    row6 :: Row,
    row7 :: Row,
    row8 :: Row,
    row9 :: Row,
    row10 :: Row,
    row11 :: Row,
    row12 :: Row,
    row13 :: Row,
    row14 :: Row,
    row15 :: Row,
    row16 :: Row,
    row17 :: Row,
    row18 :: Row,
    row19 :: Row,
    row20 :: Row,
    row21 :: Row
}

--work with the assumption for now that we don't want to vary from the tetris protocols.
-- TODO: for this dataype to be useful it needs to have a lookupsystem. So it needs a y value.
data Row =
  Row {
    cell0 :: Cell,
    cell1 :: Cell,
    cell2 :: Cell,
    cell3 :: Cell,
    cell4 :: Cell,
    cell5 :: Cell,
    cell6 :: Cell,
    cell7 :: Cell,
    cell8 :: Cell,
    cell9 :: Cell
} deriving (Eq, Show)

data Cell = Cell {
  coord   :: Pos,
  contain :: FullOrNot
} deriving (Show, Eq)

data FullOrNot = Empty | Full deriving (Show, Eq)

--list of settled tetronimos - no longer matters what shape they are, so we just have a list.
--we therefore need to handle whether a settled tetronimo is a breach of gameover before we output the new gamestate.
type SettledBlocks = [Pos]

--ordered from bottom to top, left to right.
data Tetronimo =
     Tetronimo { first  :: Pos,
                 second :: Pos,
                 third  :: Pos,
                 fourth :: Pos
    } deriving (Show)

-- data TetronimoType = iTetronimo
--                    | lTetronimo
--                    | jTetronimo
--                    | sTetronimo
--                    | zTetronimo
--                    | tTetronimo

data Pos = Pos {
  xcoord :: Int,
  ycoord :: Int
  } deriving (Show, Eq)


--test row
r :: Row
r = Row a b c d e f g h i j
  where a = Cell (Pos 1 1) Empty
        b = Cell (Pos 1 2) Full
        c = Cell (Pos 1 2) Full
        d = Cell (Pos 1 5) Full
        e = Cell (Pos 1 4) Empty
        f = Cell (Pos 1 6) Full
        g = Cell (Pos 1 7) Full
        h = Cell (Pos 1 8) Full
        i = Cell (Pos 1 9) Full
        j = Cell (Pos 1 10) Full
