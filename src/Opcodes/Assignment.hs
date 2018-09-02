module Opcodes.Assignment
( assignToRegister
) where

import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Helpers
import Types

{-
  0x8XY0
  Sets VX to the value of VY.
-}
assignToRegister :: Chip8 ()
assignToRegister = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode
      registerYValue = getRegisterYValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      storeAssignment = flip V.update $ V.fromList [(registerX,registerYValue)]
  modify (\givenState -> givenState & vRegisters %~ storeAssignment)
  incrementProgramCounter 