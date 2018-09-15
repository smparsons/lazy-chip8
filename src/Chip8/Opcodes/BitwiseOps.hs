module Chip8.Opcodes.BitwiseOps
  ( bitwiseOr
  , bitwiseAnd
  , randomBitwiseAnd
  , bitwiseXor
  , shiftRight
  , shiftLeft
  ) where

import Chip8.Helpers
import Chip8.Types
import Control.Lens
import Control.Monad.State
import Data.Bits
import qualified Data.Vector as V
import Data.Word
import System.Random

{-
  0x8XY1
  Sets VX to VX or VY. (Bitwise OR operation)
-}
bitwiseOr :: Chip8 ()
bitwiseOr = do
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue 
  let bitwiseOrResult = registerXValue .|. registerYValue
      storeBitwiseOrResult = flip V.update $ V.fromList [(registerX,bitwiseOrResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseOrResult)
  incrementProgramCounter 

{-
  0x8XY2
  Sets VX to VX and VY. (Bitwise AND operation)
-}
bitwiseAnd :: Chip8 ()
bitwiseAnd = do
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue 
  let bitwiseAndResult = registerXValue .&. registerYValue
      storeBitwiseAndResult = flip V.update $ V.fromList [(registerX,bitwiseAndResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseAndResult)
  incrementProgramCounter

{-
  0xCXNN
  Sets VX to the result of a bitwise and operation on a random number (Typically: 0 to 255) and NN.
-}
randomBitwiseAnd :: Chip8 ()
randomBitwiseAnd = do
  randomResultTuple <- fmap (randomR (0, 255)) (gets (\chip8State -> chip8State^.randomNumberSeed))
  registerX <- parseRegisterXNumber
  constant <- parseTwoDigitConstant
  let randomValue = fst randomResultTuple :: Word8
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
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue 
  let bitwiseXorResult = registerXValue `xor` registerYValue
      storeBitwiseXorResult = flip V.update $ V.fromList [(registerX,bitwiseXorResult)]
  modify (\givenState -> givenState & vRegisters %~ storeBitwiseXorResult)
  incrementProgramCounter

{-
  0x8XY6
  Stores the least significant bit of VX in VF and then shifts VX to the right by 1.
-}
shiftRight :: Chip8 ()
shiftRight = do
  registerX <- parseRegisterXNumber
  registerYValue <- getRegisterYValue 
  let leastSignificantBit = registerYValue .&. 0x1
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
  registerX <- parseRegisterXNumber
  registerY <- parseRegisterYNumber
  registerYValue <- getRegisterYValue 
  let mostSignificantBit = (registerYValue .&. 0x80) `shiftR` 7
      bitShiftResult = registerYValue `shiftL` 1
      storeBitShiftResult = flip V.update $ V.fromList
        [ (registerX,bitShiftResult)
        , (registerY,bitShiftResult)
        , (0xF,mostSignificantBit) ]
  modify (\givenState -> givenState & vRegisters %~ storeBitShiftResult)
  incrementProgramCounter