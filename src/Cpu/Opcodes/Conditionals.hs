module Cpu.Opcodes.Conditionals
( registerEqualsConstant,
  registerDoesNotEqualConstant,
  registersAreEqual,
  registersAreNotEqual
) where

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

--0x3XNN
registerEqualsConstant :: Chip8 -> Chip8
registerEqualsConstant chip8State =
  chip8State {
    programCounter = if registerXValue == constant 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    constant = parseTwoDigitConstant opcode

--0x4XNN
registerDoesNotEqualConstant :: Chip8 -> Chip8
registerDoesNotEqualConstant chip8State =
  chip8State {
    programCounter = if registerXValue /= constant 
      then originalProgramCounter + (programCounterIncrement * 2)
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    constant = parseTwoDigitConstant opcode

--0x5XY0
registersAreEqual :: Chip8 -> Chip8
registersAreEqual chip8State =
  chip8State {   
    programCounter = if registerXValue == registerYValue 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    registerYValue = getRegisterYValue opcode originalVRegisters

--0x9XY0
registersAreNotEqual :: Chip8 -> Chip8 
registersAreNotEqual chip8State =
  chip8State {   
    programCounter = if registerXValue /= registerYValue 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    registerYValue = getRegisterYValue opcode originalVRegisters