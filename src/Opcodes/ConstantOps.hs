module Opcodes.ConstantOps
( setRegisterToConstant,
  addConstantToRegister
) where

import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Helpers
import Types

{-
  0x6XNN
  Sets VX to NN.
-}
setRegisterToConstant :: Chip8 ()
setRegisterToConstant = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode
      constant = parseTwoDigitConstant $ chip8State^.currentOpcode
      storeConstant = flip V.update $ V.fromList [(registerX,constant)]
  modify (\givenState -> givenState & vRegisters %~ storeConstant)
  incrementProgramCounter 

{-
  7XNN
  Adds NN to VX. (Carry flag is not changed)
-}
addConstantToRegister :: Chip8 ()
addConstantToRegister = do
  chip8State <- get
  let registerX = parseRegisterXNumber $ chip8State^.currentOpcode
      registerXValue = getRegisterXValue (chip8State^.currentOpcode) (chip8State^.vRegisters)
      constant = parseTwoDigitConstant $ chip8State^.currentOpcode
      total = registerXValue + constant
      storeTotal = flip V.update $ V.fromList [(registerX,total)]
  modify (\givenState -> givenState & vRegisters %~ storeTotal)
  incrementProgramCounter