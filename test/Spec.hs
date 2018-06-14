module Spec where

import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "lamdalumps.Lib" $ do
    --a test for a full column, given a new tetronimo that won't fit - if you call game over on it should ouptut true
    it "returns true when a tetronimo is placed on a full playfield" $ do
      gameOver (currentTetronimo g1) `shouldBe` True
   --a test for four bottom rows that have all been filled with i's, except for the 10th, where an i tetronimo is one move away from settled -> it should return a gamestate with no settled blocks
    it "returns a gamestate with no settled blocks, when a tenth i tetronimo is slotted in amongst 9 others" $ do
      (settledTetronimos $ settle (spawn 4) g2) `shouldBe` []


 --

g1 :: Gamestate
g1 = Gamestate next current test1blocks
  where next    = spawn 4
        current = spawn 4

g2 :: Gamestate
g2 = Gamestate next current test2blocks
  where next    = spawn 4
        current = test2tet

test1blocks :: SettledBlocks
test1blocks = [Pos 4 0,  Pos 4 1,  Pos 4 2,  Pos 4 3,  Pos 4 4,
               Pos 4 5,  Pos 4 6,  Pos 4 7,  Pos 4 8,  Pos 4 9,
               Pos 4 10, Pos 4 11, Pos 4 12, Pos 4 13, Pos 4 14,
               Pos 4 15, Pos 4 16, Pos 4 17, Pos 4 18]

--9 i tetronimos, settled
test2blocks :: SettledBlocks
test2blocks = [Pos 0 0, Pos 0 1, Pos 0 2, Pos 0 3,
               Pos 1 0, Pos 1 1, Pos 1 2, Pos 1 3,
               Pos 2 0, Pos 2 1, Pos 2 2, Pos 2 3,
               Pos 3 0, Pos 3 1, Pos 3 2, Pos 3 3,
               Pos 4 0, Pos 4 1, Pos 4 2, Pos 4 3,
               Pos 5 0, Pos 5 1, Pos 5 2, Pos 5 3,
               Pos 6 0, Pos 6 1, Pos 6 2, Pos 6 3,
               Pos 7 0, Pos 7 1, Pos 7 2, Pos 7 3,
               Pos 8 0, Pos 8 1, Pos 8 2, Pos 8 3]

--i tetronimo to slot in
test2tet :: Tetronimo
test2tet =  Tetronimo (Pos 9 0) (Pos 9 1) (Pos 9 2) (Pos 9 3)

result2 :: Gamestate
result2 = Gamestate next current []
  where next = spawn 4
        current = spawn 4
