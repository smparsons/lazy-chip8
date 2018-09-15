module Chip8.Opcodes.ConditionalsSpec 
  ( spec
  ) where

import Test.Hspec

import Chip8.Opcodes.Conditionals
import Chip8
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "registerEqualsConstant" $ do
    let originalVRegisters = chip8InitialState^.vRegisters
    
    context "register and constant are equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x3530,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x5,0x30)],
        _programCounter = 0x250
      }

      let resultingState = execState registerEqualsConstant initialState
      
      it "skips the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x254

    context "register and constant are not equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x3A21,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0x5F)],
        _programCounter = 0x250
      }

      let resultingState = execState registerEqualsConstant initialState 

      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x252

  describe "registerDoesNotEqualConstant" $ do
    let originalVRegisters = chip8InitialState^.vRegisters
    
    context "register and constant are equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x3530,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x5,0x30)],
        _programCounter = 0x250
      }

      let resultingState = execState registerDoesNotEqualConstant initialState
      
      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x252

    context "register and constant are not equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x3A21,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0x5F)],
        _programCounter = 0x250
      }

      let resultingState = execState registerDoesNotEqualConstant initialState 

      it "skips the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x254

  describe "registersAreEqual" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    context "register x and y are equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x5AC0,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0x2C), (0xC, 0x2C)],
        _programCounter = 0x3A0
      }

      let resultingState = execState registersAreEqual initialState

      it "skips the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x3A4

    context "register x and y are not equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x5350,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x3, 0x11), (0x5, 0x2B)],
        _programCounter = 0x3A0
      }

      let resultingState = execState registersAreEqual initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x3A2

  describe "registersAreNotEqual" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    context "register x and y are equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x9C20,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xC, 0x25),(0x2, 0x25)],
        _programCounter = 0x355
      }

      let resultingState = execState registersAreNotEqual initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x357

    context "register x and y are not equal" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x93B0,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x3, 0x5A),(0xB, 0x71)],
        _programCounter = 0x355
      }

      let resultingState = execState registersAreNotEqual initialState

      it "skips the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x359