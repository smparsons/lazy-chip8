module Chip8.Opcodes.ConstantOps
  ( setRegisterToConstant
  , addConstantToRegister
  ) where

import Chip8.Helpers
import Chip8.Types
import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V

{-
  0x6XNN
  Sets VX to NN.
-}
setRegisterToConstant :: Chip8 ()
setRegisterToConstant = do
  registerX <- parseRegisterXNumber
  constant <- parseTwoDigitConstant
  let storeConstant = flip V.update $ V.fromList [(registerX,constant)]
  modify (\givenState -> givenState & vRegisters %~ storeConstant)
  incrementProgramCounter 

{-
  7XNN
  Adds NN to VX. (Carry flag is not changed)
-}
addConstantToRegister :: Chip8 ()
addConstantToRegister = do
  registerX <- parseRegisterXNumber
  registerXValue <- getRegisterXValue
  constant <- parseTwoDigitConstant
  let total = registerXValue + constant
      storeTotal = flip V.update $ V.fromList [(registerX,total)]
  modify (\givenState -> givenState & vRegisters %~ storeTotal)
  incrementProgramCounter