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
    else startEmulator filepath 

startEmulator :: String -> IO ()
startEmulator filepath = do
  chip8 <- initializeChip8
  chip8WithGame <- loadGameByFilePath filepath chip8
  renderer <- setupEmulatorGraphics
  emulatorLoop renderer

setupEmulatorGraphics :: IO Renderer
setupEmulatorGraphics = do
  initializeAll
  window <- createWindow "Chip-8 Emulator" defaultWindow { windowInitialSize = V2 512 256 }
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer

emulatorLoop :: Renderer -> IO ()
emulatorLoop renderer = do
  events <- pollEvents
  let userHasQuit = any isQuitEvent events
  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer
  present renderer
  unless userHasQuit (emulatorLoop renderer)

isQuitEvent :: Event -> Bool
isQuitEvent event = eventPayload event == QuitEvent