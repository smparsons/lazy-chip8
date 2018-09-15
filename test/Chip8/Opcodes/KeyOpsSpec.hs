module Chip8.Opcodes.KeyOpsSpec 
  ( spec
  ) where

import Chip8
import Chip8.Opcodes.KeyOps
import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = do
  describe "keyIsPressed" $ do
    let originalVRegisters = chip8InitialState^.vRegisters
    let originalKeyState = chip8InitialState^.keyState

    context "when key is pressed" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xEA9E,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xA, 0xC)],
        _keyState = V.update originalKeyState $ V.fromList [(0xC, Pressed)],
        _programCounter = 0x200
      }
  
      let resultingState = execState keyIsPressed initialState
      
      it "skips the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x204

    context "when key is not pressed" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xE59E,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x5, 0xA)],
        _keyState = V.update originalKeyState $ V.fromList [(0xA, Released)],
        _programCounter = 0x250
      }

      let resultingState = execState keyIsPressed initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x252

  describe "keyIsNotPressed" $ do
    let originalVRegisters = chip8InitialState^.vRegisters
    let originalKeyState = chip8InitialState^.keyState

    context "when key is not pressed" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xE7A1,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x7, 0x1)],
        _keyState = V.update originalKeyState $ V.fromList [(0x1, Released)],
        _programCounter = 0x220
      }
  
      let resultingState = execState keyIsNotPressed initialState
      
      it "skips the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x224

    context "when key is pressed" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xE2A1,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x2, 0xD)],
        _keyState = V.update originalKeyState $ V.fromList [(0xD, Pressed)],
        _programCounter = 0x27A
      }

      let resultingState = execState keyIsNotPressed initialState

      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x27C

  describe "awaitKeyPress" $ do
    let originalVRegisters = chip8InitialState^.vRegisters
    let originalKeyState = chip8InitialState^.keyState

    context "when no keys are pressed" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xF60A,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x6, 0x3B)],
        _programCounter = 0x280
      }
  
      let resultingState = execState awaitKeyPress initialState
      
      it "leaves register untouched" $ do
        let resultingVRegisters = resultingState^.vRegisters
        let register = resultingVRegisters V.! 0x6
        register `shouldBe` 0x3B

      it "does not move to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x280

    context "when key is pressed" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xF90A,
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x9, 0x7C)],
        _keyState = V.update originalKeyState $ V.fromList [(0x3, Pressed)],
        _programCounter = 0x312
      }

      let resultingState = execState awaitKeyPress initialState

      it "stores key pressed in register" $ do
        let resultingVRegisters = resultingState^.vRegisters
        let register = resultingVRegisters V.! 0x9
        register `shouldBe` 0x3

      it "continues to the next instruction" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x314