module Cpu.Opcodes.Math
( addTwoRegisters
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
