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
import Control.Monad.State
import Control.Lens

import Helpers
import Types

{-
  0x8XY1
  Sets VX to VX or VY. (Bitwise OR operation)
-}
bitwiseOr :: Chip8 ()
bitwiseOr = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode 
      registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      registerYValue = getRegisterYValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      bitwiseOrResult = registerXValue .|. registerYValue
      storeBitwiseOrResult = flip V.update $ V.fromList [(registerX,bitwiseOrResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseOrResult)
  incrementProgramCounter 

{-
  0x8XY2
  Sets VX to VX and VY. (Bitwise AND operation)
-}
bitwiseAnd :: Chip8 ()
bitwiseAnd = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode 
      registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      registerYValue = getRegisterYValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      bitwiseAndResult = registerXValue .&. registerYValue
      storeBitwiseAndResult = flip V.update $ V.fromList [(registerX,bitwiseAndResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseAndResult)
  incrementProgramCounter

{-
  0xCXNN
  Sets VX to the result of a bitwise and operation on a random number (Typically: 0 to 255) and NN.
-}
randomBitwiseAnd :: Chip8 ()
randomBitwiseAnd = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode
      constant = parseTwoDigitConstant $ chip8State^.currentOpcode
      randomResultTuple = randomR (0, 255) (chip8State^.randomNumberSeed)
      randomValue = fst randomResultTuple :: Word8
      newSeed = snd randomResultTuple
      bitwiseAndResult = constant .&. randomValue
      storeBitwiseAndResult = flip V.update $ V.fromList [(registerX,bitwiseAndResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseAndResult)
  modify (\givenState -> givenState & randomNumberSeed .~ newSeed)
  incrementProgramCounter

{-
  0x8XY3
  Sets VX to VX xor VY.
-}
bitwiseXor :: Chip8 ()
bitwiseXor = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode 
      registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      registerYValue = getRegisterYValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      bitwiseXorResult = registerXValue `xor` registerYValue
      storeBitwiseXorResult = flip V.update $ V.fromList [(registerX,bitwiseXorResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseXorResult)
  incrementProgramCounter

{-
  0x8XY6
  Stores the least significant bit of VX in VF and then shifts VX to the right by 1.
-}
shiftRight :: Chip8 ()
shiftRight = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode 
      registerYValue = getRegisterYValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      leastSignificantBit = registerYValue .&. 0x1
      bitShiftResult = registerYValue `shiftR` 1
      storeBitShiftResult = flip V.update $ V.fromList [(registerX,bitShiftResult),(0xF,leastSignificantBit)]
  modify (\givenState -> givenState & vRegisters %~ storeBitShiftResult)
  incrementProgramCounter

{-
  0x8XYE
  Stores the most significant bit of VX in VF and then shifts VX to the left by 1.
-}
shiftLeft :: Chip8 ()
shiftLeft = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode 
      registerY = parseRegisterYNumber $ chip8State^.currentOpcode
      registerYValue = getRegisterYValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      mostSignificantBit = (registerYValue .&. 0x80) `shiftR` 7
      bitShiftResult = registerYValue `shiftL` 1
      storeBitShiftResult = flip V.update $ V.fromList
        [ (registerX,bitShiftResult)
        , (registerY,bitShiftResult)
        , (0xF,mostSignificantBit) ]
  modify (\givenState -> givenState & vRegisters %~ storeBitShiftResult)
  incrementProgramCounter