{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Control.Monad.State
import Control.Concurrent
import Control.Lens hiding (Context)
import qualified Data.ByteString as BS
import Data.Maybe
import Foreign.C.Types
import qualified SDL
import Sound.ALUT
import System.Environment
import System.Random

import Constants
import Chip8
import Types

type Emulator a = StateT Chip8State IO a

hoist :: Monad m => State s a -> StateT s m a
hoist = StateT . (return .) . runState

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

setupSDLComponents :: IO (SDL.Window, SDL.Renderer, SDL.Texture)
setupSDLComponents = do
  SDL.initializeAll
  window <- SDL.createWindow "Chip-8 Emulator" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 640 320 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (SDL.V2 64 32) 
  return (window, renderer, texture) 

destroySDLComponents :: SDL.Window -> SDL.Renderer -> SDL.Texture -> IO ()
destroySDLComponents window renderer texture = do
  SDL.destroyTexture texture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

initializeChip8State :: Emulator ()
initializeChip8State = do
  newSeed <- liftIO newStdGen
  hoist $ initializeChip8 newSeed

loadGameByFilePath :: String -> Emulator ()
loadGameByFilePath filepath = do
  contents <- liftIO $ BS.readFile filepath
  let game = BS.unpack contents
  hoist $ loadGameIntoMemory game

emulatorLoop :: SDL.Renderer -> SDL.Texture -> Source -> Emulator ()
emulatorLoop renderer texture audioSource = do
  hoist emulateCpuCycle
  updatedTexture <- drawGraphicsIfNecessary renderer texture
  playBeepIfNecessary audioSource

  events <- liftIO SDL.pollEvents  
  let userHasQuit = any isQuitEvent events
      keyPressChanges = getKeyPressChanges events

  hoist $ storeKeyPressChanges keyPressChanges
  liftIO $ threadDelay 1500
  unless userHasQuit (emulatorLoop renderer updatedTexture audioSource)

getKeyPressChanges :: [SDL.Event] -> [(Int, KeyPressState)]
getKeyPressChanges = catMaybes . map getMappedKeyPressAndMotion 

getMappedKeyPressAndMotion :: SDL.Event -> Maybe (Int, KeyPressState)
getMappedKeyPressAndMotion event = 
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      case mapKeyPress keyboardEvent of         
        Just mappedKeyPress -> 
          let keyPressMotion = mapKeyPressMotion keyboardEvent in 
          Just (mappedKeyPress, keyPressMotion)
        Nothing -> Nothing
    _ -> Nothing

mapKeyPress :: SDL.KeyboardEventData -> Maybe Int
mapKeyPress keyboardEvent =
  case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
    SDL.Keycode1 -> Just 0x1
    SDL.Keycode2 -> Just 0x2
    SDL.Keycode3 -> Just 0x3
    SDL.Keycode4 -> Just 0xC
    SDL.KeycodeQ -> Just 0x4
    SDL.KeycodeW -> Just 0x5
    SDL.KeycodeE -> Just 0x6
    SDL.KeycodeR -> Just 0xD
    SDL.KeycodeA -> Just 0x7
    SDL.KeycodeS -> Just 0x8
    SDL.KeycodeD -> Just 0x9
    SDL.KeycodeF -> Just 0xE
    SDL.KeycodeZ -> Just 0xA
    SDL.KeycodeX -> Just 0x0
    SDL.KeycodeC -> Just 0xB
    SDL.KeycodeV -> Just 0xF 
    _ -> Nothing 

mapKeyPressMotion :: SDL.KeyboardEventData -> KeyPressState
mapKeyPressMotion keyboardEvent =
  case SDL.keyboardEventKeyMotion keyboardEvent of
    SDL.Pressed -> Pressed
    SDL.Released -> Released

isQuitEvent :: SDL.Event -> Bool
isQuitEvent event = SDL.eventPayload event == SDL.QuitEvent

drawGraphicsIfNecessary :: SDL.Renderer -> SDL.Texture -> Emulator SDL.Texture
drawGraphicsIfNecessary renderer texture = do
  canDrawGraphics <- gets (\givenState -> givenState^.drawFlag)
  if canDrawGraphics 
    then do 
      pixels <- hoist getGraphicsAsByteString
      updatedTexture <- liftIO $ SDL.updateTexture texture Nothing pixels (256 :: CInt)
      liftIO $ SDL.copy renderer updatedTexture Nothing Nothing
      liftIO $ SDL.present renderer
      modify (\givenState -> givenState & drawFlag .~ False)
      return updatedTexture
    else return texture

createBeepSource :: IO Source
createBeepSource = do
  beep <- createBuffer $ Sine 340 0 0.075
  [source] <- genObjectNames 1
  queueBuffers source [beep]
  return source

playBeepIfNecessary :: Source -> Emulator ()
playBeepIfNecessary beep = do
  shouldPlayBeep <- gets (\givenState -> givenState^.audioFlag)
  if shouldPlayBeep 
    then do
      liftIO $ play [beep]
      modify (\givenState -> givenState & audioFlag .~ False)
    else return ()