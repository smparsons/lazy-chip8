module Chip8.CpuSpec 
( spec
) where

import Test.Hspec
import Chip8
import Chip8.Cpu
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "extractOpcodeFromMemory" $ do
    let chip8State = chip8InitialState {
      _memory = V.update (V.replicate 4096 0x00) $ V.fromList 
        [ (0x240,0x8A)
        , (0x241,0x74)
        , (0x242,0x00)
        , (0x243,0xEE)
        , (0x244,0x05)
        , (0x245,0x72)
        , (0x246,0xFA)
        , (0x247,0x29) ]
    }

    it "correctly extracts opcodes" $ do
      let firstResult = execState extractOpcodeFromMemory chip8State { _programCounter = 0x240 }
      (firstResult^.currentOpcode) `shouldBe` 0x8A74
      let secondResult = execState extractOpcodeFromMemory chip8State { _programCounter = 0x242 }
      (secondResult^.currentOpcode) `shouldBe` 0x00EE
      let thirdResult = execState extractOpcodeFromMemory chip8State { _programCounter = 0x244 }
      (thirdResult^.currentOpcode) `shouldBe` 0x0572
      let fourthResult = execState extractOpcodeFromMemory chip8State { _programCounter = 0x246 }
      (fourthResult^.currentOpcode) `shouldBe` 0xFA29

  describe "parseDigitsFromOpcode" $ do
    it "correctly parses first, third, and fourth digits from opcode" $ do
      evalState parseDigitsFromOpcode chip8InitialState { _currentOpcode = 0x85B4 } `shouldBe` (0x8, 0xB, 0x4) 
      evalState parseDigitsFromOpcode chip8InitialState { _currentOpcode = 0x00EE } `shouldBe` (0x0, 0xE, 0xE) 
      evalState parseDigitsFromOpcode chip8InitialState { _currentOpcode = 0x00E0 } `shouldBe` (0x0, 0xE, 0x0)
      evalState parseDigitsFromOpcode chip8InitialState { _currentOpcode = 0xC352 } `shouldBe` (0xC, 0x5, 0x2) 
      evalState parseDigitsFromOpcode chip8InitialState { _currentOpcode = 0xFA29 } `shouldBe` (0xF, 0x2, 0x9)

  describe "decrementSoundTimer" $ do
    it "decrements sound timer" $ do
      let initialState = chip8InitialState { _soundTimer = 0x21 }
      let resultingState = execState decrementSoundTimer initialState
      let resultingSoundTimer = resultingState^.soundTimer
      let resultingAudioFlag = resultingState^.audioFlag

      resultingAudioFlag `shouldBe` False
      resultingSoundTimer `shouldBe` 0x20

    it "sets audio flag to true when decrementing from one to zero" $ do
      let initialState = chip8InitialState { _soundTimer = 0x1 }
      let resultingState = execState decrementSoundTimer initialState
      let resultingSoundTimer = resultingState^.soundTimer
      let resultingAudioFlag = resultingState^.audioFlag

      resultingAudioFlag `shouldBe` True
      resultingSoundTimer `shouldBe` 0x0

    it "does not decrement sound timer when it is already at zero" $ do
      let initialState = chip8InitialState { _soundTimer = 0x0 }
      let resultingState = execState decrementSoundTimer initialState
      let resultingSoundTimer = resultingState^.soundTimer
      let resultingAudioFlag = resultingState^.audioFlag

      resultingAudioFlag `shouldBe` False
      resultingSoundTimer `shouldBe` 0x0

  describe "decrementDelayTimer" $ do
    it "decrements delay timer" $ do
      let initialState = chip8InitialState { _delayTimer = 0x1A }
      let resultingState = execState decrementDelayTimer initialState
      let resultingDelayTimer = resultingState^.delayTimer
      resultingDelayTimer `shouldBe` 0x19

    it "does not decrement delay timer when it is already at zero" $ do
      let initialState = chip8InitialState { _delayTimer = 0x0 }
      let resultingState = execState decrementDelayTimer initialState
      let resultingDelayTimer = resultingState^.delayTimer
      resultingDelayTimer `shouldBe` 0x0
      