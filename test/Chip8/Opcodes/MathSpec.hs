module Chip8.Opcodes.MathSpec 
  ( spec
  ) where

import Chip8
import Chip8.Opcodes.Math
import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = do
  describe "addTwoRegisters" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    context "without carry" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x83C4,  
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x3,0x5A),(0xC,0x13)],
        _programCounter = 0x260
      } 
      let resultingState = execState addTwoRegisters initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "adds register y to register x" $ do
        let total = resultingVRegisters V.! 0x3
        total `shouldBe` 0x6D

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "increases program counter by 2" $ do
        let resultingProgramCounter = resultingState^.programCounter 
        resultingProgramCounter `shouldBe` 0x262

    context "with carry" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x8B44,  
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xB,0xC3),(0x4,0xB2)],
        _programCounter = 0x3A1
      } 
      let resultingState = execState addTwoRegisters initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "adds register y to register x" $ do
        let total = resultingVRegisters V.! 0xB
        total `shouldBe` 0x75

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "increases program counter by 2" $ do
        let resultingProgramCounter = resultingState^.programCounter
        resultingProgramCounter `shouldBe` 0x3A3

  describe "subtractRegister" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    context "without borrow" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x8145,  
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x1,0x74),(0x4,0x3B)],
        _programCounter = 0x4C0
      } 

      let resultingState = execState subtractRegister initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "subtracts register y from register x" $ do
        let difference = resultingVRegisters V.! 0x1
        difference `shouldBe` 0x39

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "increases program counter by 2" $ do
        let resultingProgramCounter = resultingState^.programCounter
        resultingProgramCounter `shouldBe` 0x4C2

    context "with borrow" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x8CA5,  
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x25),(0xA,0x5C)],
        _programCounter = 0x7CB
      } 
      let resultingState = execState subtractRegister initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "subtracts register y from register x" $ do
        let difference = resultingVRegisters V.! 0xC
        difference `shouldBe` 0xC9

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "increases program counter by 2" $ do
        let resultingProgramCounter = resultingState^.programCounter
        resultingProgramCounter `shouldBe` 0x7CD

  describe "subtractTwoRegisters" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    context "without borrow" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x8AE7,  
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xA,0x43),(0xE,0x8B)],
        _programCounter = 0x4B5
      } 
      let resultingState = execState subtractTwoRegisters initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "subtracts register x from reigster y" $ do
        let difference = resultingVRegisters V.! 0xA
        difference `shouldBe` 0x48

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "increases program counter by 2" $ do
        let resultingProgramCounter = resultingState^.programCounter
        resultingProgramCounter `shouldBe` 0x4B7

    context "with borrow" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x8C27,  
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x98),(0x2,0x4F)],
        _programCounter = 0x6AA
      } 
      let resultingState = execState subtractTwoRegisters initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "subtracts register x from reigster y" $ do
        let difference = resultingVRegisters V.! 0xC
        difference `shouldBe` 0xB7

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "increases program counter by 2" $ do
        let resultingProgramCounter = resultingState^.programCounter
        resultingProgramCounter `shouldBe` 0x6AC