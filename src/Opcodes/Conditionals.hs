module Opcodes.Conditionals
( registerEqualsConstant,
  registerDoesNotEqualConstant,
  registersAreEqual,
  registersAreNotEqual
) where

import Helpers
import Types
import Constants

{-
  0x3XNN
  Skips the next instruction if VX equals NN. (Usually the next instruction is a jump to 
  skip a code block)
-}
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

{-
  0x4XNN
  Skips the next instruction if VX doesn't equal NN. (Usually the next instruction is a jump 
  to skip a code block)
-}
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

{-
  0x5XY0
  Skips the next instruction if VX equals VY. (Usually the next instruction is a jump 
  to skip a code block)
-}
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

{-
  0x9XY0
  Skips the next instruction if VX doesn't equal VY. (Usually the next instruction is a 
  jump to skip a code block)
-}
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