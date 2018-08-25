module Opcodes.AssignmentSpec 
( spec
) where

import Test.Hspec

import Opcodes.Assignment
import Types
import TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "assignToRegister" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x8C50,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x25),(0x5,0xA1)],
      programCounter = 0x27A
    }

    let resultingState = assignToRegister initialState

    it "assigns register y to register x" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0xC
      register `shouldBe` 0xA1

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x27C