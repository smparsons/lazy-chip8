module Opcodes.KeyOpsSpec 
( spec
) where

import Test.Hspec

import Opcodes.KeyOps
import Types
import Constants
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "keyIsPressed" $ do
    let originalVRegisters = vRegisters chip8InitialState
    let originalKeyState = keyState chip8InitialState 

    context "when key is pressed" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0xEA9E,
        vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0xC)],
        keyState = V.update originalKeyState $ V.fromList [(0xC, 0x1)],
        programCounter = 0x200
      }
  
      let resultingState = keyIsPressed initialState
      
      it "skips the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x204

    context "when key is not pressed" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0xE59E,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x5, 0xA)],
        keyState = V.update originalKeyState $ V.fromList [(0xA, 0x0)],
        programCounter = 0x250
      }

      let resultingState = keyIsPressed initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x252

  describe "keyIsNotPressed" $ do
    let originalVRegisters = vRegisters chip8InitialState
    let originalKeyState = keyState chip8InitialState 

    context "when key is not pressed" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0xE7A1,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x7, 0x1)],
        keyState = V.update originalKeyState $ V.fromList [(0x1, 0x0)],
        programCounter = 0x220
      }
  
      let resultingState = keyIsNotPressed initialState
      
      it "skips the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x224

    context "when key is pressed" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0xE2A1,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x2, 0xD)],
        keyState = V.update originalKeyState $ V.fromList [(0xD, 0x1)],
        programCounter = 0x27A
      }

      let resultingState = keyIsNotPressed initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x27C

  describe "awaitKeyPress" $ do
    let originalVRegisters = vRegisters chip8InitialState
    let originalKeyState = keyState chip8InitialState 

    context "when no keys are pressed" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0xF60A,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x6, 0x3B)],
        programCounter = 0x280
      }
  
      let resultingState = awaitKeyPress initialState
      
      it "leaves register untouched" $ do
        let resultingVRegisters = vRegisters resultingState
        let register = resultingVRegisters V.! 0x6
        register `shouldBe` 0x3B

      it "does not move to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x280

    context "when key is pressed" $ do
      let initialState = chip8InitialState {
        currentOpcode = 0xF90A,
        vRegisters = V.update originalVRegisters $ V.fromList [(0x9, 0x7C)],
        keyState = V.update originalKeyState $ V.fromList [(0x3, 0x1)],
        programCounter = 0x312
      }

      let resultingState = awaitKeyPress initialState

      it "stores key pressed in register" $ do
        let resultingVRegisters = vRegisters resultingState
        let register = resultingVRegisters V.! 0x9
        register `shouldBe` 0x3

      it "continues to the next instruction" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x314