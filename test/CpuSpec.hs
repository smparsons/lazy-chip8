module CpuSpec 
( spec
) where

import Test.Hspec
import Cpu
import Types
import TestHelpers
import Data.Word
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "extractOpcodeFromMemory" $ do
    let emptyMemory = V.replicate 4096 0x00
    let updatedMemory = V.update emptyMemory $ V.fromList 
          [ (0x240,0x8A)
          , (0x241,0x74)
          , (0x242,0x00)
          , (0x243,0xEE)
          , (0x244,0x05)
          , (0x245,0x72)
          , (0x246,0xFA)
          , (0x247,0x29) ]

    it "correctly extracts opcodes" $ do
      let firstOpcode = extractOpcodeFromMemory updatedMemory 0x240      
      firstOpcode `shouldBe` 0x8A74
      let secondOpcode = extractOpcodeFromMemory updatedMemory 0x242
      secondOpcode `shouldBe` 0x00EE
      let thirdOpcode = extractOpcodeFromMemory updatedMemory 0x244
      thirdOpcode `shouldBe` 0x0572
      let fourthOpcode = extractOpcodeFromMemory updatedMemory 0x246
      fourthOpcode `shouldBe` 0xFA29

  describe "parseDigitsFromOpcode" $ do
    it "correctly parses first, third, and fourth digits from opcode" $ do
      parseDigitsFromOpcode 0x85B4 `shouldBe` (0x8, 0xB, 0x4) 
      parseDigitsFromOpcode 0x00EE `shouldBe` (0x0, 0xE, 0xE) 
      parseDigitsFromOpcode 0x00E0 `shouldBe` (0x0, 0xE, 0x0)
      parseDigitsFromOpcode 0xC352 `shouldBe` (0xC, 0x5, 0x2) 
      parseDigitsFromOpcode 0xFA29 `shouldBe` (0xF, 0x2, 0x9)

  describe "decrementSoundTimer" $ do
    it "decrements sound timer" $ do
      let initialState = defaultState { soundTimer = 0x21 }
      let resultingState = decrementSoundTimer initialState
      let resultingSoundTimer = soundTimer resultingState
      resultingSoundTimer `shouldBe` 0x20

    it "does not decrement sound timer when it is already at zero" $ do
      let initialState = defaultState { soundTimer = 0x0 }
      let resultingState = decrementSoundTimer initialState
      let resultingSoundTimer = soundTimer resultingState
      resultingSoundTimer `shouldBe` 0x0

  describe "decrementDelayTimer" $ do
    it "decrements delay timer" $ do
      let initialState = defaultState { delayTimer = 0x1A }
      let resultingState = decrementDelayTimer initialState
      let resultingDelayTimer = delayTimer resultingState
      resultingDelayTimer `shouldBe` 0x19

    it "does not decrement delay timer when it is already at zero" $ do
      let initialState = defaultState { delayTimer = 0x0 }
      let resultingState = decrementDelayTimer initialState
      let resultingDelayTimer = delayTimer resultingState
      resultingDelayTimer `shouldBe` 0x0
      