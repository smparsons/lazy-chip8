module Opcodes.KeyOps
( keyIsPressed,
  keyIsNotPressed,
  awaitKeyPress
) where

import Data.Word
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Helpers
import Types

{-
  0xEX9E
  Skips the next instruction if the key stored in VX is pressed. (Usually the next instruction 
  is a jump to skip a code block)
-}
keyIsPressed :: Chip8 ()
keyIsPressed = do
  chip8State <- get 
  let registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      key = fromIntegral registerXValue :: Int
      keyValue = (chip8State^.keyState) V.! key
  if keyValue == 0x1 then skipNextInstruction else incrementProgramCounter

{-
  0xEXA1
  Skips the next instruction if the key stored in VX isn't pressed. (Usually the next instruction 
  is a jump to skip a code block)
-}
keyIsNotPressed :: Chip8 ()
keyIsNotPressed = do
  chip8State <- get
  let registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      key = fromIntegral registerXValue :: Int
      keyValue = (chip8State^.keyState) V.! key
  if keyValue == 0x0 then skipNextInstruction else incrementProgramCounter

{-
  0xFX0A
  A key press is awaited, and then stored in VX. (Blocking Operation. All instruction halted 
  until next key event)
-}
awaitKeyPress :: Chip8 ()
awaitKeyPress = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode
      pressedKey = V.findIndex (\key -> key == 0x1) (chip8State^.keyState)
  case pressedKey of
    Nothing -> return ()
    Just key -> do
      let convertedKey = fromIntegral key :: Word8
          storeKeyPress = flip V.update $ V.fromList [(registerX,convertedKey)]
      modify (\givenState -> givenState & vRegisters %~ storeKeyPress)
      incrementProgramCounter