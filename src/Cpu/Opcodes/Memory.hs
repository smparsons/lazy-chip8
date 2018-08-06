module Cpu.Opcodes.Memory
( setIndexRegisterToAddress
) where

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0xANNN
setIndexRegisterToAddress :: Chip8 -> Chip8 
setIndexRegisterToAddress chip8State =
  chip8State {
    indexRegister = address,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    address = parseThreeDigitConstant opcode