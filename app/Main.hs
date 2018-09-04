{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import qualified SDL
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import System.Random
import Foreign.C.Types
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Hashable

import Constants
import Chip8
import Types

instance Hashable SDL.Keycode

chip8KeyMapping :: M.HashMap SDL.Keycode Int
chip8KeyMapping = M.fromList
  [ (SDL.Keycode1, 0x1)
  , (SDL.Keycode2, 0x2)
  , (SDL.Keycode3, 0x3)
  , (SDL.Keycode4, 0xC)
  , (SDL.KeycodeQ, 0x4)
  , (SDL.KeycodeW, 0x5)
  , (SDL.KeycodeE, 0x6)
  , (SDL.KeycodeR, 0xD)
  , (SDL.KeycodeA, 0x7)
  , (SDL.KeycodeS, 0x8)
  , (SDL.KeycodeD, 0x9)
  , (SDL.KeycodeF, 0xE)
  , (SDL.KeycodeZ, 0xA)
  , (SDL.KeycodeX, 0x0)
  , (SDL.KeycodeC, 0xB)
  , (SDL.KeycodeV, 0xF) ]

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

  SDL.initializeAll
  window <- SDL.createWindow "Chip-8 Emulator" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 640 320 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (SDL.V2 64 32)  
  
  emulatorLoop chip8State' renderer texture

  SDL.destroyTexture texture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

initializeChip8State :: IO Chip8State
initializeChip8State = do
  newSeed <- newStdGen
  return $ execState (initializeChip8 newSeed) chip8InitialState

loadGameByFilePath :: String -> Chip8State -> IO Chip8State
loadGameByFilePath filepath chip8State = do
  contents <- BS.readFile filepath
  let game = BS.unpack contents
  return $ execState (loadGameIntoMemory game) chip8State

emulatorLoop :: Chip8State -> SDL.Renderer -> SDL.Texture -> IO ()
emulatorLoop chip8State renderer texture = do
  let updatedChip8State = execState emulateCpuCycle chip8State
  updatedTexture <- drawGraphicsIfApplicable updatedChip8State renderer texture

  events <- SDL.pollEvents
  let userHasQuit = any isQuitEvent events
      keyPressChanges = getKeyPressChanges events
      chip8StateWithKeyPresses = execState (storeKeyPressChanges keyPressChanges) updatedChip8State

  threadDelay 1200

  unless userHasQuit (emulatorLoop chip8StateWithKeyPresses renderer updatedTexture)

getKeyPressChanges :: [SDL.Event] -> [(Int, KeyPressState)]
getKeyPressChanges = catMaybes . map getMappingAndKeyPressState 

getMappingAndKeyPressState :: SDL.Event -> Maybe (Int, KeyPressState)
getMappingAndKeyPressState event = 
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent -> do
      case M.lookup (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)) chip8KeyMapping of
        Just keyMapping -> 
          case SDL.keyboardEventKeyMotion keyboardEvent of
            SDL.Pressed -> Just (keyMapping, Pressed)
            SDL.Released -> Just (keyMapping, Released)
        Nothing -> Nothing
    _ -> Nothing 

isQuitEvent :: SDL.Event -> Bool
isQuitEvent event = SDL.eventPayload event == SDL.QuitEvent

drawGraphicsIfApplicable :: Chip8State -> SDL.Renderer -> SDL.Texture -> IO SDL.Texture
drawGraphicsIfApplicable chip8State renderer texture =
  if (chip8State^.drawFlag) then drawGraphics chip8State renderer texture else return texture

drawGraphics :: Chip8State -> SDL.Renderer -> SDL.Texture -> IO SDL.Texture
drawGraphics chip8State renderer texture = do
  let pixels = evalState getGraphicsAsByteString chip8State
  updatedTexture <- SDL.updateTexture texture Nothing pixels (256 :: CInt)
  SDL.copy renderer updatedTexture Nothing Nothing
  SDL.present renderer
  return updatedTexture