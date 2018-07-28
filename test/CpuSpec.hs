module CpuSpec 
( spec
) where

import Test.Hspec
import Cpu
import qualified Data.Vector as V

defaultState :: Chip8
defaultState = Chip8 {
  currentOpcode = 0x0000,
  memory = V.empty,
  vRegisters = V.replicate 16 0x00,
  indexRegister = 0x0000,
  programCounter = 0x0000,
  graphics = V.replicate 2048 0x00,
  delayTimer = 0x00,
  soundTimer = 0x00,
  stack = V.empty,
  stackPointer = 0x0000,
  keyState = V.empty
}

spec :: Spec
spec = do
  describe "clearScreen" $ do
    let initialState = defaultState {
      graphics = V.replicate 2048 0x1
    }
    let resultingState = clearScreen initialState

    it "clears the screen" $ do
      let updatedGraphics = graphics resultingState
      let expectedGraphics = V.replicate 2048 0x0
      updatedGraphics `shouldBe` expectedGraphics

  describe "returnFromSubroutine" $ do
    let initialState = defaultState {
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
    let initialState = defaultState {
      currentOpcode = 0x11EF,
      programCounter = 0x3FF
    }
    let resultingState = jumpToAddress initialState 

    it "jumps to address 1NN" $ do
      let currentProgramCounter = programCounter resultingState 
      currentProgramCounter `shouldBe` 0x1EF

  describe "callSubroutine" $ do
    let initialState = defaultState { 
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