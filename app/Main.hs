{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SDL
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Control.Monad.State
import Control.Concurrent
import System.Random

import Constants
import Chip8
import Emulator.Events
import Emulator.Graphics
import Emulator.Helpers
import Emulator.Types

main :: IO ()
main = do
  args <- getArgs  
  let filepath = head args
  if null args 
    then putStrLn "Please provide a filepath to the chip8 game." 
    else evalStateT (startEmulator filepath) chip8InitialState

startEmulator :: String -> Emulator ()
startEmulator filepath = do
  initializeChip8State
  loadGameByFilePath filepath

  liftIO initializeAll
  window <- liftIO $ createWindow "Chip-8 Emulator" defaultWindow { windowInitialSize = V2 640 320 }
  renderer <- liftIO $ createRenderer window (-1) defaultRenderer
  texture <- liftIO $ createTexture renderer RGBA8888 TextureAccessStatic (V2 64 32)  
  
  emulatorLoop renderer texture

  destroyTexture texture
  destroyRenderer renderer
  destroyWindow window

initializeChip8State :: Emulator ()
initializeChip8State = do
  newSeed <- liftIO newStdGen
  hoist $ initializeChip8 newSeed

loadGameByFilePath :: String -> Emulator ()
loadGameByFilePath filepath = do
  contents <- liftIO $ BS.readFile filepath
  let game = BS.unpack contents
  hoist $ loadGameIntoMemory game

emulatorLoop :: Renderer -> Texture -> Emulator ()
emulatorLoop renderer texture = do
  hoist emulateCpuCycle
  updatedTexture <- drawGraphicsIfApplicable renderer texture

  events <- liftIO pollEvents
  let userHasQuit = any isQuitEvent events
      keyPressChanges = getKeyPressChanges events

  hoist $ storeKeyPressChanges keyPressChanges
  liftIO $ threadDelay 1200
  unless userHasQuit (emulatorLoop renderer updatedTexture)