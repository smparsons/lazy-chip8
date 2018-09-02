module Opcodes.Timer
( setRegisterToDelayTimer,
  setDelayTimerToRegister,
  setSoundTimerToRegister 
) where

import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Helpers
import Types

{-
  0xFX07
  Sets VX to the value of the delay timer.
-}
setRegisterToDelayTimer :: Chip8 ()
setRegisterToDelayTimer = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode
      updateRegisterX = flip V.update $ V.fromList [(registerX, chip8State^.delayTimer)]
  modify (\givenState -> givenState & vRegisters %~ updateRegisterX)
  incrementProgramCounter

{-
  0xFX15
  Sets the delay timer to VX.
-}
setDelayTimerToRegister :: Chip8 ()
setDelayTimerToRegister = do
  chip8State <- get
  let registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
  modify (\givenState -> givenState & delayTimer .~ registerXValue)
  incrementProgramCounter

{-
  0xFX18
  Sets the sound timer to VX.
-}
setSoundTimerToRegister :: Chip8 () 
setSoundTimerToRegister = do
  chip8State <- get
  let registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
  modify (\givenState -> givenState & soundTimer .~ registerXValue)
  incrementProgramCounter