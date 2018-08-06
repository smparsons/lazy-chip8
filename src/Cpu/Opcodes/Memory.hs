module Cpu.Opcodes.Memory
( setIndexRegisterToAddress,
  addRegisterToIndexRegister
) where

import Data.Word

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

--0xFX1E
addRegisterToIndexRegister :: Chip8 -> Chip8 
addRegisterToIndexRegister chip8State =
  chip8State {
    indexRegister = indexRegisterValue + convertedRegisterXValue,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    indexRegisterValue = indexRegister chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    convertedRegisterXValue = fromIntegral registerXValue :: Word16