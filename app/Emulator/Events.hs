module Emulator.Events 
  ( getKeyPressChanges
  , isQuitEvent
  ) where

import Data.Maybe
import qualified SDL

import Chip8

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