module Cpu.Opcodes.KeyOps
( keyIsPressed,
  keyIsNotPressed
) where

import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0xEX9E
keyIsPressed :: Chip8 -> Chip8 
keyIsPressed chip8State =
  chip8State {
    programCounter = if keyValue == 0x1 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State
    originalKeyState = keyState chip8State  
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    key = (fromIntegral registerXValue) :: Int
    keyValue = originalKeyState V.! key

--0xEXA1
keyIsNotPressed :: Chip8 -> Chip8
keyIsNotPressed chip8State = 
  chip8State {
    programCounter = if keyValue == 0x0 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State
    originalKeyState = keyState chip8State  
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    key = (fromIntegral registerXValue) :: Int
    keyValue = originalKeyState V.! key
