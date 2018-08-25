module Opcodes.ConstantOpsSpec 
( spec
) where

import Test.Hspec

import Opcodes.ConstantOps
import Types
import Constants
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "setRegisterToConstant" $ do
    let originalVRegisters = vRegisters chip8InitialState

    let initialState = chip8InitialState {
      currentOpcode = 0x6C23,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xC, 0x5A)],
      programCounter = 0x180
    }

    let resultingState = setRegisterToConstant initialState

    it "sets register to constant" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0xC
      register `shouldBe` 0x23

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState 
      updatedProgramCounter `shouldBe` 0x182

  describe "addConstantToRegister" $ do
    let originalVRegisters = vRegisters chip8InitialState

    let initialState = chip8InitialState {
      currentOpcode = 0x7EA2,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xE, 0x15)],
      programCounter = 0x210
    }

    let resultingState = addConstantToRegister initialState

    it "adds constant to register" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0xE
      register `shouldBe` 0xB7

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x212