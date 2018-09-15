module Chip8.Opcodes.FlowSpec 
( spec
) where

import Test.Hspec

import Chip8.Opcodes.Flow
import Chip8
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "returnFromSubroutine" $ do
    let initialState = chip8InitialState {
      _stackPointer = 2,
      _stack = V.fromList [0x150, 0x2F2],
      _programCounter = 0x316
    }
    let resultingState = execState returnFromSubroutine initialState

    it "removes the last address from the stack" $ do
      let latestAddressInStack = V.last $ resultingState^.stack
      latestAddressInStack `shouldBe` 0x150

    it "decrements the stack pointer" $ do
      let resultingStackLength = V.length $ resultingState^.stack
      resultingStackLength `shouldBe` 1

    it "returns from the subroutine" $ do
      let resultingProgramCounter = resultingState^.programCounter
      resultingProgramCounter `shouldBe` 0x2F4

  describe "jumpToAddress" $ do
    let initialState = chip8InitialState {
      _currentOpcode = 0x11EF,
      _programCounter = 0x3FF
    }
    let resultingState = execState jumpToAddress initialState 

    it "jumps to address NNN" $ do
      let currentProgramCounter = resultingState^.programCounter
      currentProgramCounter `shouldBe` 0x1EF

  describe "callSubroutine" $ do
    let initialState = chip8InitialState { 
      _currentOpcode = 0x225F,
      _stackPointer = 1,
      _stack = V.fromList [0x210],
      _programCounter = 0x220
    }
    let resultingState = execState callSubroutine initialState

    it "stores the current address in the stack" $ do
      let latestAddressInStack = V.last $ resultingState^.stack
      latestAddressInStack `shouldBe` 0x220

    it "increases the length of the stack by one" $ do
      let resultingStackLength = V.length $ resultingState^.stack
      resultingStackLength `shouldBe` 2

    it "increments the stack pointer" $ do
      let resultingStackPointer = resultingState^.stackPointer
      resultingStackPointer `shouldBe` 2

    it "jumps to the given address" $ do
      let resultingProgramCounter = resultingState^.programCounter
      resultingProgramCounter `shouldBe` 0x25F 

  describe "jumpToAddressPlusRegisterZero" $ do 
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xB1FA,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0x0,0x51)],
      _programCounter = 0x12A
    }
    let resultingState = execState jumpToAddressPlusRegisterZero initialState 

    it "jumps to address NNN + V0" $ do
      let currentProgramCounter = resultingState^.programCounter
      currentProgramCounter `shouldBe` 0x24B