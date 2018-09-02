{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SDL
import Linear (V4(..))
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Control.Monad.State
import System.Random

import Constants
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
  newSeed <- newStdGen
  let chip8State = execState (initializeChip8 newSeed) chip8InitialState
  contents <- BS.readFile filepath
  let game = BS.unpack contents
  let chip8State' = execState (loadGameIntoMemory game) chip8State
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