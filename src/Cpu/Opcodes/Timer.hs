module Cpu.Opcodes.Timer
( setRegisterToDelayTimer,
  setDelayTimerToRegister,
  setSoundTimerToRegister 
) where

import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0xFX07
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

--0xFX15
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

--0xFX18
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

