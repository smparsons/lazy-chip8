module Chip8.Opcodes.TimerSpec 
( spec
) where

import Test.Hspec

import Chip8.Opcodes.Timer
import Chip8
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "setRegisterToDelayTimer" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xFD07,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0xD,0x15)],
      _delayTimer = 0x2A,
      _programCounter = 0x15B
    }

    let resultingState = execState setRegisterToDelayTimer initialState

    it "assigns register x to the value of the delay timer" $ do
      let resultingVRegisters = resultingState^.vRegisters
      let register = resultingVRegisters V.! 0xD
      register `shouldBe` 0x2A

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x15D

  describe "setDelayTimerToRegister" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xF815,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0x8,0x10)],
      _delayTimer = 0x31,
      _programCounter = 0x18A
    }

    let resultingState = execState setDelayTimerToRegister initialState

    it "asigns the delay timer to the value stored in register x" $ do
      let resultingDelayTimer = resultingState^.delayTimer
      resultingDelayTimer `shouldBe` 0x10

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x18C 

  describe "setSoundTimerToRegister" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xF218,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0x2,0x2C)],
      _soundTimer = 0x33,
      _programCounter = 0x12C
    }

    let resultingState = execState setSoundTimerToRegister initialState

    it "assigns the sound timer to the value stored in register x" $ do
      let resultingSoundTimer = resultingState^.soundTimer
      resultingSoundTimer `shouldBe` 0x2C

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x12E