module Opcodes.Timer
( setRegisterToDelayTimer,
  setDelayTimerToRegister,
  setSoundTimerToRegister 
) where

import qualified Data.Vector as V

import Helpers
import Types
import Constants

{-
  0xFX07
  Sets VX to the value of the delay timer.
-}
setRegisterToDelayTimer :: Chip8 -> Chip8
setRegisterToDelayTimer chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,delayTimerValue)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    delayTimerValue = delayTimer chip8State 

{-
  0xFX15
  Sets the delay timer to VX.
-}
setDelayTimerToRegister :: Chip8 -> Chip8 
setDelayTimerToRegister chip8State =
  chip8State {
    delayTimer = registerXValue,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerXValue = getRegisterXValue opcode originalVRegisters

{-
  0xFX18
  Sets the sound timer to VX.
-}
setSoundTimerToRegister :: Chip8 -> Chip8 
setSoundTimerToRegister chip8State =
  chip8State {
    soundTimer = registerXValue,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerXValue = getRegisterXValue opcode originalVRegisters

