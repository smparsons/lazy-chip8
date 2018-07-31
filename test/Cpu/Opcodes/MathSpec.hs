module Cpu.Opcodes.MathSpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.Math
import Cpu.Types
import Cpu.TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "addTwoRegisters" $ do
    let originalVRegisters = vRegisters defaultState

    context "without carry" $ do
      let initialState = defaultState {
        currentOpcode = 0x83C4,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0x3,0x5A),(0xC,0x13)],
        programCounter = 0x260
      } 
      let resultingState = addTwoRegisters initialState
      let resultingVRegisters = vRegisters resultingState

      it "adds register y to register x" $ do
        let sum = resultingVRegisters V.! 0x3
        sum `shouldBe` 0x6D

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "increases program counter by 2" $ do
        let resultingProgramCounter = programCounter resultingState 
        resultingProgramCounter `shouldBe` 0x262

    context "with carry" $ do
      let initialState = defaultState {
        currentOpcode = 0x8B44,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0xB,0xC3),(0x4,0xB2)],
        programCounter = 0x3A1
      } 
      let resultingState = addTwoRegisters initialState
      let resultingVRegisters = vRegisters resultingState

      it "adds register y to register x" $ do
        let sum = resultingVRegisters V.! 0xB
        sum `shouldBe` 0x75

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "increases program counter by 2" $ do
        let resultingProgramCounter = programCounter resultingState 
        resultingProgramCounter `shouldBe` 0x3A3