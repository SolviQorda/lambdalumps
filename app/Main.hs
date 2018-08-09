module Main where

import Gloss.Render
import RhineGloss.GameBuilder

import Data.Char

--Run Lambdalumps from the console with rhine-gloss
--TODO: give console option to run with gloss or rhine-gloss

main :: IO ()
main = do
  putStrLn "-----------------------------------------------------------"
  putStrLn "----------------- Welcome to LambdaLumps! -----------------"
  putStrLn "-----------------------------------------------------------"
  putStrLn "---- (a shameless ripoff of Tetris built with Haskell) ----"
  putStrLn "-----------------------------------------------------------"
  putStrLn "----------------------by Solvi Naja------------------------"
  putStrLn "-----------------------------------------------------------"
  putStrLn "> press 'r' to load the game with rhine-gloss, or 'g' to load the game with just gloss"
  backend <- getLine
  putStrLn "  CONTROLS:"
  putStrLn "                                                           "
  putStrLn "> arrow keys to move lumps left and right"
  putStrLn "> tab to rotate tetronimos"
  putStrLn "> enter to drop"
  putStrLn "> backspace to hold"
  -- putStrLn "> d to increase difficulty"
  putStrLn "> p to pause"
  handleBackend backend
  -- commenting this out for now, until I find a way to pass IO to the rhine gloss


handleBackend :: String -> IO ()
handleBackend s
  |s == "r" = do
    putStrLn $ "> starting LambdaLumps at level " ++ (parseDifficultyInput "1")
    putStrLn "> Press any key to start."
    anyKey <- getLine
    renderGameRhineGloss
  | otherwise = do
    putStrLn "                                                           "
    putStrLn "> enter a number between 1 and 9 to set starting difficulty"
    difficulty <- getLine
    putStrLn "> Press any key to start."
    anyKey <- getLine
    renderGameGloss . digitToInt $ head $ parseDifficultyInput difficulty


parseDifficultyInput :: String -> String
parseDifficultyInput input
  | input `elem` ["1","2","3","4","5","6","7","8","9"] = input
  | otherwise                                          = "1"
