{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SDL
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import System.Random
import Foreign.C.Types

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
  chip8State <- initializeChip8State
  chip8State' <- loadGameByFilePath filepath chip8State

  initializeAll
  window <- createWindow "Chip-8 Emulator" defaultWindow { windowInitialSize = V2 640 320 }
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer RGBA8888 TextureAccessStatic (V2 64 32)  
  
  emulatorLoop chip8State' renderer texture

  destroyTexture texture
  destroyRenderer renderer
  destroyWindow window

initializeChip8State :: IO Chip8State
initializeChip8State = do
  newSeed <- newStdGen
  return $ execState (initializeChip8 newSeed) chip8InitialState

loadGameByFilePath :: String -> Chip8State -> IO Chip8State
loadGameByFilePath filepath chip8State = do
  contents <- BS.readFile filepath
  let game = BS.unpack contents
  return $ execState (loadGameIntoMemory game) chip8State

emulatorLoop :: Chip8State -> Renderer -> Texture -> IO ()
emulatorLoop chip8State renderer texture = do
  let updatedChip8State = execState emulateCpuCycle chip8State
  updatedTexture <- drawGraphicsIfApplicable updatedChip8State renderer texture
  threadDelay 1200

  events <- pollEvents
  let userHasQuit = any isQuitEvent events

  unless userHasQuit (emulatorLoop updatedChip8State renderer updatedTexture)

isQuitEvent :: Event -> Bool
isQuitEvent event = eventPayload event == QuitEvent

drawGraphicsIfApplicable :: Chip8State -> Renderer -> Texture -> IO Texture
drawGraphicsIfApplicable chip8State renderer texture =
  if (chip8State^.drawFlag) then drawGraphics chip8State renderer texture else return texture

drawGraphics :: Chip8State -> Renderer -> Texture -> IO Texture
drawGraphics chip8State renderer texture = do
  let pixels = evalState getGraphicsAsByteString chip8State
  updatedTexture <- updateTexture texture Nothing pixels (256 :: CInt)
  copy renderer updatedTexture Nothing Nothing
  present renderer
  return updatedTexture