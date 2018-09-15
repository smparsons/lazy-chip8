module Chip8.Opcodes.Timer
  ( setRegisterToDelayTimer
  , setDelayTimerToRegister
  , setSoundTimerToRegister 
  ) where

import Chip8.Helpers
import Chip8.Types
import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V

{-
  0xFX07
  Sets VX to the value of the delay timer.
-}
setRegisterToDelayTimer :: Chip8 ()
setRegisterToDelayTimer = do
  currentDelayTimer <- gets (\chip8State -> chip8State^.delayTimer)
  registerX <- parseRegisterXNumber
  let updateRegisterX = flip V.update $ V.fromList [(registerX, currentDelayTimer)]
  modify (\givenState -> givenState & vRegisters %~ updateRegisterX)
  incrementProgramCounter

{-
  0xFX15
  Sets the delay timer to VX.
-}
setDelayTimerToRegister :: Chip8 ()
setDelayTimerToRegister = do
  registerXValue <- getRegisterXValue
  modify (\givenState -> givenState & delayTimer .~ registerXValue)
  incrementProgramCounter

{-
  0xFX18
  Sets the sound timer to VX.
-}
setSoundTimerToRegister :: Chip8 () 
setSoundTimerToRegister = do
  registerXValue <- getRegisterXValue
  modify (\givenState -> givenState & soundTimer .~ registerXValue)
  incrementProgramCounter