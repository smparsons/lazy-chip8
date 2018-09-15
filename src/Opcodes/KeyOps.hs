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
  currentKeyState <- gets (\chip8State -> chip8State^.keyState)
  registerXValue <- fmap fromIntegral getRegisterXValue 
  let keyMotion = currentKeyState V.! registerXValue
  skipNextInstructionIf $ keyMotion == Pressed

{-
  0xEXA1
  Skips the next instruction if the key stored in VX isn't pressed. (Usually the next instruction 
  is a jump to skip a code block)
-}
keyIsNotPressed :: Chip8 ()
keyIsNotPressed = do
  currentKeyState <- gets (\chip8State -> chip8State^.keyState)
  registerXValue <- fmap fromIntegral getRegisterXValue
  let keyMotion = currentKeyState V.! registerXValue
  skipNextInstructionIf $ keyMotion == Released

{-
  0xFX0A
  A key press is awaited, and then stored in VX. (Blocking Operation. All instruction halted 
  until next key event)
-}
awaitKeyPress :: Chip8 ()
awaitKeyPress = do
  currentKeyState <- gets (\chip8State -> chip8State^.keyState)
  registerX <- parseRegisterXNumber
  let pressedKey = V.findIndex (\key -> key == Pressed) currentKeyState
  case pressedKey of
    Nothing -> return ()
    Just key -> do
      let convertedKey = fromIntegral key :: Word8
          storeKeyPress = flip V.update $ V.fromList [(registerX,convertedKey)]
      modify (\givenState -> givenState & vRegisters %~ storeKeyPress)
      incrementProgramCounter