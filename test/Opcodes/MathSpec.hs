module Opcodes.MathSpec 
( spec
) where

import Test.Hspec

import Opcodes.Math
import Types
import Constants
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "addTwoRegisters" $ do
    let originalVRegisters = vRegisters chip8InitialState

    context "without carry" $ do
      let initialState = chip8InitialState {
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
      let initialState = chip8InitialState {
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

  describe "subtractRegister" $ do
    let originalVRegisters = vRegisters chip8InitialState

    context "without borrow" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0x8145,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0x1,0x74),(0x4,0x3B)],
        programCounter = 0x4C0
      } 

      let resultingState = subtractRegister initialState
      let resultingVRegisters = vRegisters resultingState

      it "subtracts register y from register x" $ do
        let difference = resultingVRegisters V.! 0x1
        difference `shouldBe` 0x39

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "increases program counter by 2" $ do
        let resultingProgramCounter = programCounter resultingState 
        resultingProgramCounter `shouldBe` 0x4C2

    context "with borrow" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0x8CA5,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x25),(0xA,0x5C)],
        programCounter = 0x7CB
      } 
      let resultingState = subtractRegister initialState
      let resultingVRegisters = vRegisters resultingState

      it "subtracts register y from register x" $ do
        let difference = resultingVRegisters V.! 0xC
        difference `shouldBe` 0xC9

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "increases program counter by 2" $ do
        let resultingProgramCounter = programCounter resultingState 
        resultingProgramCounter `shouldBe` 0x7CD

  describe "subtractTwoRegisters" $ do
    let originalVRegisters = vRegisters chip8InitialState

    context "without borrow" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0x8AE7,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0xA,0x43),(0xE,0x8B)],
        programCounter = 0x4B5
      } 
      let resultingState = subtractTwoRegisters initialState
      let resultingVRegisters = vRegisters resultingState

      it "subtracts register x from reigster y" $ do
        let difference = resultingVRegisters V.! 0xA
        difference `shouldBe` 0x48

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "increases program counter by 2" $ do
        let resultingProgramCounter = programCounter resultingState 
        resultingProgramCounter `shouldBe` 0x4B7

    context "with borrow" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0x8C27,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x98),(0x2,0x4F)],
        programCounter = 0x6AA
      } 
      let resultingState = subtractTwoRegisters initialState
      let resultingVRegisters = vRegisters resultingState

      it "subtracts register x from reigster y" $ do
        let difference = resultingVRegisters V.! 0xC
        difference `shouldBe` 0xB7

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "increases program counter by 2" $ do
        let resultingProgramCounter = programCounter resultingState 
        resultingProgramCounter `shouldBe` 0x6AC