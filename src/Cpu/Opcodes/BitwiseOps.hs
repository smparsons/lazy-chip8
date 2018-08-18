module Cpu.Opcodes.BitwiseOps
( bitwiseOr,
  bitwiseAnd,
  bitwiseXor,
  shiftRight,
  shiftLeft
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

--0x8XY6
shiftRight :: Chip8 -> Chip8 
shiftRight chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList 
      [ (registerX,bitShiftResult)
      , (0xF,leastSignificantBit) ],
    programCounter = originalProgramCounter + programCounterIncrement
  } 
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    registerYValue = getRegisterYValue opcode originalVRegisters
    leastSignificantBit = registerYValue .&. 0x1
    bitShiftResult = shiftR registerYValue 1

--0x8XYE
shiftLeft :: Chip8 -> Chip8 
shiftLeft chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList 
      [ (registerX,bitShiftResult)
      , (registerY,bitShiftResult)
      , (0xF,mostSignificantBit) ],
    programCounter = originalProgramCounter + programCounterIncrement
  } 
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    registerY = parseRegisterYNumber opcode
    registerYValue = getRegisterYValue opcode originalVRegisters
    mostSignificantBit = shiftR (registerYValue .&. 0x80) 7
    bitShiftResult = shiftL registerYValue 1