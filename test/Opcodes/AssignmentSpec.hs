module Opcodes.AssignmentSpec 
( spec
) where

import Test.Hspec

import Opcodes.Assignment
import Types
import Constants
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "assignToRegister" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0x8C50,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x25),(0x5,0xA1)],
      _programCounter = 0x27A
    }

    let resultingState = execState assignToRegister initialState

    it "assigns register y to register x" $ do
      let resultingVRegisters = resultingState^.vRegisters
      let register = resultingVRegisters V.! 0xC
      register `shouldBe` 0xA1

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x27C