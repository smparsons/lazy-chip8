module Cpu.Opcodes.Assignment
( assignToRegister
) where

import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0x8XY0
assignToRegister :: Chip8 -> Chip8 
assignToRegister chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,registerYValue)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    registerYValue = getRegisterYValue opcode originalVRegisters