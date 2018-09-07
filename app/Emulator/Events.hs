module Emulator.Events
( getKeyPressChanges,
  isQuitEvent
) where

import qualified SDL
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Hashable

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