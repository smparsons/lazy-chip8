module Opcodes.Assignment
( assignToRegister
) where

import qualified Data.Vector as V

import Helpers
import Types
import Constants

{-
  0x8XY0
  Sets VX to the value of VY.
-}
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