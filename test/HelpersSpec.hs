module HelpersSpec 
( spec
) where

import Test.Hspec
import Helpers
import Data.Word
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Types
import Constants

emptyVRegisters :: V.Vector Word8
emptyVRegisters = V.replicate 16 0x00

spec :: Spec
spec = do
  describe "parseRegisterXNumber" $ do
    it "parses X out of the opcode" $ do
      let registerXNumber = evalState parseRegisterXNumber chip8InitialState { _currentOpcode = 0x28A0 }
      registerXNumber `shouldBe` 0x8

  describe "parseRegisterYNumber" $ do
    it "parses Y out of the opcode" $ do
      let registerYNumber = evalState parseRegisterYNumber chip8InitialState { _currentOpcode = 0x13CF }
      registerYNumber `shouldBe` 0xC

  describe "getRegisterXValue" $ do
    it "returns the value stored in register X" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x5AC0,
        _vRegisters = V.update emptyVRegisters $ V.fromList [(0xA,0x3C)]
      }
      let registerXValue = evalState getRegisterXValue initialState
      registerXValue `shouldBe` 0x3C

  describe "getRegisterYValue" $ do
    it "returns the value stored in register Y" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0x3720,
        _vRegisters = V.update emptyVRegisters $ V.fromList [(0x2,0x1F)]
      }
      let registerYValue = evalState getRegisterYValue initialState
      registerYValue `shouldBe` 0x1F

  describe "parseOnDigitConstant" $ do
    it "returns the last digit from the opcode" $ do
      let constant = evalState parseOneDigitConstant chip8InitialState { _currentOpcode = 0xD5B4 }
      constant `shouldBe` 0x4

  describe "parseTwoDigitConstant" $ do   
    it "returns the last two digits from the opcode" $ do
      let constant = evalState parseTwoDigitConstant chip8InitialState { _currentOpcode = 0x5A27 }
      constant `shouldBe` 0x27

  describe "parseThreeDigitConstant" $ do
    it "returns the last three digits from the opcode" $ do
      let constant = evalState parseThreeDigitConstant chip8InitialState { _currentOpcode = 0x4AC3 } 
      constant `shouldBe` 0xAC3

  describe "incrementProgramCounter" $ do
    it "increments the program counter" $ do
      let updatedState = execState incrementProgramCounter chip8InitialState { _programCounter = 0x240 }
      (updatedState^.programCounter) `shouldBe` 0x242

  describe "skipNextInstruction" $ do
    it "skips the next instruction" $ do
      let updatedState = execState skipNextInstruction chip8InitialState { _programCounter = 0x280 }
      (updatedState^.programCounter) `shouldBe` 0x284