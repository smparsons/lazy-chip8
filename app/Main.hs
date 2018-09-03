{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SDL
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Control.Monad.State
import System.Random
import Data.Word
import Foreign.C.Types

import Constants
import Chip8

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

  initializeAll
  window <- createWindow "Chip-8 Emulator" defaultWindow { windowInitialSize = V2 640 320 }
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer RGBA8888 TextureAccessStatic (V2 64 32)  
  
  emulatorLoop renderer texture

  destroyTexture texture
  destroyRenderer renderer
  destroyWindow window

emulatorLoop :: Renderer -> Texture -> IO ()
emulatorLoop renderer texture = do
  events <- pollEvents
  let userHasQuit = any isQuitEvent events
  drawGraphics renderer texture
  unless userHasQuit (emulatorLoop renderer texture)

isQuitEvent :: Event -> Bool
isQuitEvent event = eventPayload event == QuitEvent

drawGraphics :: Renderer -> Texture -> IO ()
drawGraphics renderer texture = do
  let black = [0, 0, 0, 0] :: [Word8]
      white = [255, 255, 255, 255] :: [Word8]
      pixelArray = (flatten [black,white,black,white]) ++ (take 8160 (cycle black)) ++ (flatten [black,white,black,white])
      pixels = BS.pack pixelArray

  updatedTexture <- updateTexture texture Nothing pixels (256 :: CInt)
  copy renderer updatedTexture Nothing Nothing
  present renderer

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []