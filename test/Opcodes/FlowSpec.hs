module Opcodes.FlowSpec 
( spec
) where

import Test.Hspec

import Opcodes.Flow
import Types
import Constants
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "returnFromSubroutine" $ do
    let initialState = chip8InitialState {
      stackPointer = 2,
      stack = V.fromList [0x150, 0x2F2],
      programCounter = 0x316
    }
    let resultingState = returnFromSubroutine initialState

    it "removes the last address from the stack" $ do
      let latestAddressInStack = V.last $ stack resultingState 
      latestAddressInStack `shouldBe` 0x150

    it "decrements the stack pointer" $ do
      let resultingStackLength = V.length $ stack resultingState
      resultingStackLength `shouldBe` 1

    it "returns from the subroutine" $ do
      let resultingProgramCounter = programCounter resultingState 
      resultingProgramCounter `shouldBe` 0x2F4

  describe "jumpToAddress" $ do
    let initialState = chip8InitialState {
      currentOpcode = 0x11EF,
      programCounter = 0x3FF
    }
    let resultingState = jumpToAddress initialState 

    it "jumps to address NNN" $ do
      let currentProgramCounter = programCounter resultingState 
      currentProgramCounter `shouldBe` 0x1EF

  describe "callSubroutine" $ do
    let initialState = chip8InitialState { 
      currentOpcode = 0x225F,
      stackPointer = 1,
      stack = V.fromList [0x210],
      programCounter = 0x220
    }
    let resultingState = callSubroutine initialState

    it "stores the current address in the stack" $ do
      let latestAddressInStack = V.last $ stack resultingState
      latestAddressInStack `shouldBe` 0x220

    it "increases the length of the stack by one" $ do
      let resultingStackLength = V.length $ stack resultingState
      resultingStackLength `shouldBe` 2

    it "increments the stack pointer" $ do
      let resultingStackPointer = stackPointer resultingState
      resultingStackPointer `shouldBe` 2

    it "jumps to the given address" $ do
      let resultingProgramCounter = programCounter resultingState
      resultingProgramCounter `shouldBe` 0x25F 

  describe "jumpToAddressPlusRegisterZero" $ do 
    let originalVRegisters = vRegisters chip8InitialState

    let initialState = chip8InitialState {
      currentOpcode = 0xB1FA,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x0,0x51)],
      programCounter = 0x12A
    }
    let resultingState = jumpToAddressPlusRegisterZero initialState 

    it "jumps to address NNN + V0" $ do
      let currentProgramCounter = programCounter resultingState 
      currentProgramCounter `shouldBe` 0x24B