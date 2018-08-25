module Main where

import System.Environment

import Chip8
import Types

main :: IO ()
main = do
  args <- getArgs  
  let filepath = head args
  if null args 
    then putStrLn "Please provide a filepath to the chip8 game." 
    else startChip8Game filepath 

startChip8Game :: String -> IO ()
startChip8Game filepath = do
  chip8 <- initializeChip8
  chip8WithGame <- loadGameByFilePath filepath chip8
  print chip8WithGame
