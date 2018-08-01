module Cpu.Opcodes.ConditionalsSpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.Conditionals
import Cpu.Types
import Cpu.TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "registerEqualsConstant" $ do
    let originalVRegisters = vRegisters defaultState
    
    context "register and constant are equal" $ do
      let initialState = defaultState {
        currentOpcode = 0x3530,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x5,0x30)],
        programCounter = 0x250
      }

      let resultingState = registerEqualsConstant initialState
      
      it "skips the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x254

    context "register and constant are not equal" $ do
      let initialState = defaultState {
        currentOpcode = 0x3A21,
        vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0x5F)],
        programCounter = 0x250
      }

      let resultingState = registerEqualsConstant initialState 

      it "continues to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState 
        updatedProgramCounter `shouldBe` 0x252

  describe "registerDoesNotEqualConstant" $ do
    let originalVRegisters = vRegisters defaultState
    
    context "register and constant are equal" $ do
      let initialState = defaultState {
        currentOpcode = 0x3530,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x5,0x30)],
        programCounter = 0x250
      }

      let resultingState = registerDoesNotEqualConstant initialState
      
      it "continues to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x252

    context "register and constant are not equal" $ do
      let initialState = defaultState {
        currentOpcode = 0x3A21,
        vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0x5F)],
        programCounter = 0x250
      }

      let resultingState = registerDoesNotEqualConstant initialState 

      it "skips the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState 
        updatedProgramCounter `shouldBe` 0x254

  describe "registersAreEqual" $ do
    let originalVRegisters = vRegisters defaultState

    context "register x and y are equal" $ do
      let initialState = defaultState {
        currentOpcode = 0x5AC0,
        vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0x2C), (0xC, 0x2C)],
        programCounter = 0x3A0
      }

      let resultingState = registersAreEqual initialState

      it "skips the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x3A4

    context "register x and y are not equal" $ do
      let initialState = defaultState {
        currentOpcode = 0x5350,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x3, 0x11), (0x5, 0x2B)],
        programCounter = 0x3A0
      }

      let resultingState = registersAreEqual initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x3A2