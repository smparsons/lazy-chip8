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
  graphics = V.empty,
  delayTimer = 0x00,
  soundTimer = 0x00,
  stack = V.empty,
  stackPointer = 0x0000,
  keyState = V.empty
}

spec :: Spec
spec = do
  describe "executeOpcode2NNN" $ do
    let initialState = defaultState { 
      currentOpcode = 0x225F,
      stackPointer = 1,
      stack = V.fromList [0x210],
      programCounter = 0x220
    }
    let resultingState = executeOpcode2NNN initialState

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

  describe "executeOpcode8XY4" $ do
    let originalVRegisters = vRegisters defaultState

    context "without carry" $ do
      let initialState = defaultState {
        currentOpcode = 0x83C4,  
        vRegisters = V.update originalVRegisters $ V.fromList [(0x3,0x5A),(0xC,0x13)],
        programCounter = 0x260
      } 
      let resultingState = executeOpcode8XY4 initialState
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
      let resultingState = executeOpcode8XY4 initialState
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