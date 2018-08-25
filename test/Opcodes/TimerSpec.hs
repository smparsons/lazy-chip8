module Opcodes.TimerSpec 
( spec
) where

import Test.Hspec

import Opcodes.Timer
import Types
import Constants
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "setRegisterToDelayTimer" $ do
    let originalVRegisters = vRegisters chip8InitialState

    let initialState = chip8InitialState {
      currentOpcode = 0xFD07,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xD,0x15)],
      delayTimer = 0x2A,
      programCounter = 0x15B
    }

    let resultingState = setRegisterToDelayTimer initialState

    it "assigns register x to the value of the delay timer" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0xD
      register `shouldBe` 0x2A

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x15D

  describe "setDelayTimerToRegister" $ do
    let originalVRegisters = vRegisters chip8InitialState

    let initialState = chip8InitialState {
      currentOpcode = 0xF815,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x8,0x10)],
      delayTimer = 0x31,
      programCounter = 0x18A
    }

    let resultingState = setDelayTimerToRegister initialState

    it "asigns the delay timer to the value stored in register x" $ do
      let resultingDelayTimer = delayTimer resultingState
      resultingDelayTimer `shouldBe` 0x10

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x18C 

  describe "setSoundTimerToRegister" $ do
    let originalVRegisters = vRegisters chip8InitialState

    let initialState = chip8InitialState {
      currentOpcode = 0xF218,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x2,0x2C)],
      soundTimer = 0x33,
      programCounter = 0x12C
    }

    let resultingState = setSoundTimerToRegister initialState

    it "assigns the sound timer to the value stored in register x" $ do
      let resultingSoundTimer = soundTimer resultingState
      resultingSoundTimer `shouldBe` 0x2C

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x12E