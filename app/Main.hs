{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SDL
import Linear (V4(..))
import Control.Monad (unless)

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
  setupChip8GameWindow

setupChip8GameWindow :: IO ()
setupChip8GameWindow = do
  initializeAll
  window <- createWindow "Chip-8 Emulator" defaultWindow { windowInitialSize = V2 512 256 }
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let userHasQuit = any isQuitEvent events
  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer
  present renderer
  unless userHasQuit (appLoop renderer)

isQuitEvent :: Event -> Bool
isQuitEvent event = eventPayload event == QuitEvent