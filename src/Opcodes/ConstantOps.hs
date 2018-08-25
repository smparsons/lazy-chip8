module Opcodes.ConstantOps
( setRegisterToConstant,
  addConstantToRegister
) where

import qualified Data.Vector as V

import Helpers
import Types
import Constants

{-
  0x6XNN
  Sets VX to NN.
-}
setRegisterToConstant :: Chip8 -> Chip8 
setRegisterToConstant chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,constant)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    constant = parseTwoDigitConstant opcode

{-
  7XNN
  Adds NN to VX. (Carry flag is not changed)
-}
addConstantToRegister :: Chip8 -> Chip8
addConstantToRegister chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList[(registerX,total)],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    registerXValue = getRegisterXValue opcode originalVRegisters
    constant = parseTwoDigitConstant opcode
    total = registerXValue + constant