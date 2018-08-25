module Opcodes.BitwiseOps
( bitwiseOr,
  bitwiseAnd,
  randomBitwiseAnd,
  bitwiseXor,
  shiftRight,
  shiftLeft
) where

import System.Random
import Data.Word
import Data.Bits
import qualified Data.Vector as V

import Helpers
import Types
import Constants

{-
  0x8XY1
  Sets VX to VX or VY. (Bitwise OR operation)
-}
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

{-
  0x8XY2
  Sets VX to VX and VY. (Bitwise AND operation)
-}
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

{-
  0xCXNN
  Sets VX to the result of a bitwise and operation on a random number (Typically: 0 to 255) and NN.
-}
randomBitwiseAnd :: Chip8 -> Chip8
randomBitwiseAnd chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,bitwiseAndResult)],
    randomNumberSeed = newSeed,
    programCounter = originalProgramCounter + programCounterIncrement
  }  
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    originalSeed = randomNumberSeed chip8State
    opcode = currentOpcode chip8State 
    registerX = parseRegisterXNumber opcode 
    constant = parseTwoDigitConstant opcode
    randomResultTuple = randomR (0, 255) originalSeed
    randomValue = fst randomResultTuple :: Word8
    newSeed = snd randomResultTuple
    bitwiseAndResult = constant .&. randomValue

{-
  0x8XY3
  Sets VX to VX xor VY.
-}
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

{-
  0x8XY6
  Stores the least significant bit of VX in VF and then shifts VX to the right by 1.
-}
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

{-
  0x8XYE
  Stores the most significant bit of VX in VF and then shifts VX to the left by 1.[3]
-}
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