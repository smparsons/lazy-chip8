module Cpu.Opcodes.MemorySpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.Memory
import Cpu.Types
import Cpu.TestHelpers

spec :: Spec
spec = do
  describe "setIndexRegisterToAddress" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0xA2F0,
      programCounter = 0x2AC
    }

    let resultingState = setIndexRegisterToAddress initialState

    it "sets index register to three digit address NNN" $ do
      let indexRegisterValue = indexRegister resultingState 
      indexRegisterValue `shouldBe` 0x2F0

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState 
      updatedProgramCounter `shouldBe` 0x2AE