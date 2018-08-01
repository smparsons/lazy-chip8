module Cpu.Opcodes.ConstantOps
( setRegisterToConstant,
  addConstantToRegister
) where

import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0x6XNN
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

--7XNN
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