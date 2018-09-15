{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chip8
import Control.Monad (unless)
import Control.Monad.State
import Control.Concurrent
import qualified Data.ByteString as BS
import Emulator
import SDL
import Sound.ALUT
import System.Environment
import System.Random

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
  (window, renderer, texture) <- liftIO setupSDLComponents 

  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []

  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
    currentContext $= Just context
    audioSource <- liftIO createBeepSource
    emulatorLoop renderer texture audioSource

  _ <- closeDevice device
  liftIO $ destroySDLComponents window renderer texture

setupSDLComponents :: IO (Window, Renderer, Texture)
setupSDLComponents = do
  initializeAll
  window <- createWindow "Chip-8 Emulator" defaultWindow { windowInitialSize = V2 640 320 }
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer RGBA8888 TextureAccessStatic (V2 64 32) 
  return (window, renderer, texture) 

destroySDLComponents :: Window -> Renderer -> Texture -> IO ()
destroySDLComponents window renderer texture = do
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

emulatorLoop :: Renderer -> Texture -> Source -> Emulator ()
emulatorLoop renderer texture audioSource = do
  hoist emulateCpuCycle
  updatedTexture <- drawGraphicsIfNecessary renderer texture
  playBeepIfNecessary audioSource

  events <- liftIO pollEvents  
  let userHasQuit = any isQuitEvent events
      keyPressChanges = getKeyPressChanges events

  hoist $ storeKeyPressChanges keyPressChanges
  liftIO $ threadDelay 1500
  unless userHasQuit (emulatorLoop renderer updatedTexture audioSource)