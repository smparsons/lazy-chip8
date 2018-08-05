module Cpu.Opcodes.BitwiseOps
( bitwiseOr,
  bitwiseAnd,
  bitwiseXor
) where

import Data.Bits
import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0x8XY1
bitwiseOr :: Chip8 -> Chip8 
bitwiseOr chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,bitwiseOrResult)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    registerXValue = getRegisterXValue opcode originalVRegisters
    registerYValue = getRegisterYValue opcode originalVRegisters 
    bitwiseOrResult = registerXValue .|. registerYValue 

--0x8XY2
bitwiseAnd :: Chip8 -> Chip8
bitwiseAnd chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,bitwiseAndResult)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    registerXValue = getRegisterXValue opcode originalVRegisters
    registerYValue = getRegisterYValue opcode originalVRegisters 
    bitwiseAndResult = registerXValue .&. registerYValue 

--0x8XY3
bitwiseXor :: Chip8 -> Chip8
bitwiseXor chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,bitwiseXorResult)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    registerXValue = getRegisterXValue opcode originalVRegisters
    registerYValue = getRegisterYValue opcode originalVRegisters 
    bitwiseXorResult = registerXValue `xor` registerYValue 

