module Main where

import Render

import Data.Char


main :: IO ()
main = do
  putStrLn "-----------------------------------------------------------"
  putStrLn "----------------- Welcome to LambdaLumps! -----------------"
  putStrLn "-----------------------------------------------------------"
  putStrLn "---- (a shameless ripoff of Tetris built with Haskell) ----"
  putStrLn "-----------------------------------------------------------"
  putStrLn "----------------------by Solvi Naja------------------------"
  putStrLn "-----------------------------------------------------------"
  putStrLn "  CONTROLS:"
  putStrLn "                                                           "
  putStrLn "> arrow keys to move lumps left and right"
  putStrLn "> tab to rotate tetronimos"
  putStrLn "> enter to drop"
  putStrLn "> d to increase difficulty"
  putStrLn "                                                           "
  putStrLn "> enter a number between 1 and 9 to set starting difficulty"
  difficulty <- getLine
  putStrLn $ "> starting LambdaLumps at level " ++ (parseDifficultyInput difficulty)
  renderGame . digitToInt $ head $ parseDifficultyInput difficulty

parseDifficultyInput :: String -> String
parseDifficultyInput input
  | input `elem` ["1","2","3","4","5","6","7","8","9"] = input
  | otherwise                                          = "3"
