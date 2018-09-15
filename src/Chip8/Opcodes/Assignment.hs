module Chip8.Opcodes.Assignment
  ( assignToRegister
  ) where

import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Chip8.Helpers
import Chip8.Types

{-
  0x8XY0
  Sets VX to the value of VY.
-}
assignToRegister :: Chip8 ()
assignToRegister = do
  registerX <- parseRegisterXNumber
  registerYValue <- getRegisterYValue
  let storeAssignment = flip V.update $ V.fromList [(registerX,registerYValue)]
  modify (\givenState -> givenState & vRegisters %~ storeAssignment)
  incrementProgramCounter 