module Chip8.Opcodes.ConstantOpsSpec 
( spec
) where

import Test.Hspec

import Chip8.Opcodes.ConstantOps
import Chip8
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "setRegisterToConstant" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0x6C23,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0xC, 0x5A)],
      _programCounter = 0x180
    }

    let resultingState = execState setRegisterToConstant initialState

    it "sets register to constant" $ do
      let resultingVRegisters = resultingState^.vRegisters
      let register = resultingVRegisters V.! 0xC
      register `shouldBe` 0x23

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x182

  describe "addConstantToRegister" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0x7EA2,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0xE, 0x15)],
      _programCounter = 0x210
    }

    let resultingState = execState addConstantToRegister initialState

    it "adds constant to register" $ do
      let resultingVRegisters = resultingState^.vRegisters
      let register = resultingVRegisters V.! 0xE
      register `shouldBe` 0xB7

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x212