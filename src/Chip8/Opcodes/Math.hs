module Chip8.Opcodes.Math
( addTwoRegisters,
  subtractRegister,
  subtractTwoRegisters
) where

import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Chip8.Helpers
import Chip8.Types

{-
  0x8XY4
  Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
-}
addTwoRegisters :: Chip8 ()
addTwoRegisters = do
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue
  let total = registerXValue + registerYValue
      carry = if registerYValue > (0xFF - registerXValue) then 0x1 else 0x0
      storeTotalAndCarry = flip V.update $ V.fromList [(registerX,total),(0xF,carry)]
  modify (\givenState -> givenState & vRegisters %~ storeTotalAndCarry)
  incrementProgramCounter

{-
  0x8XY5
  VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
-}
subtractRegister :: Chip8 ()
subtractRegister = do
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue
  let difference = registerXValue - registerYValue
      borrow = if registerYValue > registerXValue then 0x0 else 0x1
      storeDifferenceAndBorrow = flip V.update $ V.fromList [(registerX,difference),(0xF,borrow)]
  modify (\givenState -> givenState & vRegisters %~ storeDifferenceAndBorrow)
  incrementProgramCounter 

{-
  0x8XY7
  Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
-}
subtractTwoRegisters :: Chip8 ()
subtractTwoRegisters = do
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  registerYValue <- getRegisterYValue  
  let difference = registerYValue - registerXValue
      borrow = if registerXValue > registerYValue then 0x0 else 0x1
      storeDifferenceAndBorrow = flip V.update $ V.fromList [(registerX,difference),(0xF,borrow)]
  modify (\givenState -> givenState & vRegisters %~ storeDifferenceAndBorrow)
  incrementProgramCounter