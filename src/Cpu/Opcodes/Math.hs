module Cpu.Opcodes.Math
( addTwoRegisters,
  subtractRegister,
  subtractTwoRegisters
) where

import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0x8XY4
addTwoRegisters :: Chip8 -> Chip8
addTwoRegisters chip8State = 
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,total),(0xF,carry)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where
    originalVRegisters = vRegisters chip8State 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    registerY = parseRegisterYNumber opcode
    registerXValue = originalVRegisters V.! registerX
    registerYValue = originalVRegisters V.! registerY
    total = registerXValue + registerYValue
    carry = if registerYValue > (0xFF - registerXValue) then 0x1 else 0x0

--0x8XY5
subtractRegister :: Chip8 -> Chip8 
subtractRegister chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,difference),(0xF,borrow)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where
    originalVRegisters = vRegisters chip8State 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    registerXValue = getRegisterXValue opcode originalVRegisters 
    registerYValue = getRegisterYValue opcode originalVRegisters
    difference = registerXValue - registerYValue
    borrow = if registerYValue > registerXValue then 0x0 else 0x1

--0x8XY7
subtractTwoRegisters :: Chip8 -> Chip8
subtractTwoRegisters chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,difference),(0xF,borrow)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where
    originalVRegisters = vRegisters chip8State 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    registerXValue = getRegisterXValue opcode originalVRegisters 
    registerYValue = getRegisterYValue opcode originalVRegisters
    difference = registerYValue - registerXValue
    borrow = if registerXValue > registerYValue then 0x0 else 0x1