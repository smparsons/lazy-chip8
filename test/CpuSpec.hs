module CpuSpec 
( spec
) where

import Test.Hspec
import Cpu

defaultState :: Chip8
defaultState = Chip8 {
  currentOpcode = 0x0000,
  memory = [],
  vRegisters = [],
  indexRegister = 0x0000,
  programCounter = 0x0000,
  graphics = [],
  delayTimer = 0x00,
  soundTimer = 0x00,
  stack = [],
  stackPointer = 0x0000,
  keyState = []
}

spec :: Spec
spec =
  describe "executeOpcode2NNN" $ do
    let initialState = defaultState { 
      currentOpcode = 0x225F,
      stackPointer = 1,
      stack = [ 0x210 ],
      programCounter = 0x220
    }
    let resultingState = executeOpcode2NNN initialState

    it "stores the current address in the stack" $ do
      let latestAddressInStack = last $ stack resultingState
      latestAddressInStack `shouldBe` 0x220

    it "increases the length of the stack by one" $ do
      let resultingStackLength = length $ stack resultingState
      resultingStackLength `shouldBe` 2

    it "increments the stack pointer" $ do
      let resultingStackPointer = stackPointer resultingState
      resultingStackPointer `shouldBe` 2

    it "jumps to the given address" $ do
      let resultingProgramCounter = programCounter resultingState
      resultingProgramCounter `shouldBe` 0x25F 