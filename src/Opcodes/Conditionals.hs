module Opcodes.Conditionals
( registerEqualsConstant,
  registerDoesNotEqualConstant,
  registersAreEqual,
  registersAreNotEqual
) where

import Helpers
import Types

{-
  0x3XNN
  Skips the next instruction if VX equals NN. (Usually the next instruction is a jump to 
  skip a code block)
-}
registerEqualsConstant :: Chip8 ()
registerEqualsConstant = do
  registerXValue <- getRegisterXValue
  constant <- parseTwoDigitConstant
  if registerXValue == constant then skipNextInstruction else incrementProgramCounter

{-
  0x4XNN
  Skips the next instruction if VX doesn't equal NN. (Usually the next instruction is a jump 
  to skip a code block)
-}
registerDoesNotEqualConstant :: Chip8 ()
registerDoesNotEqualConstant = do
  registerXValue <- getRegisterXValue
  constant <- parseTwoDigitConstant
  if registerXValue /= constant then skipNextInstruction else incrementProgramCounter

{-
  0x5XY0
  Skips the next instruction if VX equals VY. (Usually the next instruction is a jump 
  to skip a code block)
-}
registersAreEqual :: Chip8 ()
registersAreEqual = do
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue
  if registerXValue == registerYValue then skipNextInstruction else incrementProgramCounter

{-
  0x9XY0
  Skips the next instruction if VX doesn't equal VY. (Usually the next instruction is a 
  jump to skip a code block)
-}
registersAreNotEqual :: Chip8 () 
registersAreNotEqual = do
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue
  if registerXValue /= registerYValue then skipNextInstruction else incrementProgramCounter