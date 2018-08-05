module Cpu.Opcodes.BitwiseOpsSpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.BitwiseOps
import Cpu.Types
import Cpu.TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "bitwiseOr" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x8AB1,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xA,0x7A),(0xB,0x05)],
      programCounter = 0x1FE
    }

    let resultingState = bitwiseOr initialState

    it "assigns register x to the result of doing a bitwise or on register x and y" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0xA
      register `shouldBe` 0x7F

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x200

  describe "bitwiseAnd" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x8372,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x3,0x70),(0x7,0x10)],
      programCounter = 0x3A0
    }

    let resultingState = bitwiseAnd initialState

    it "assigns register x to the result of doing a bitwise and on register x and y" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0x3
      register `shouldBe` 0x10

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x3A2

  describe "bitwiseXor" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x89D3,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x9,0x1A),(0xD,0x1F)],
      programCounter = 0x27C
    }

    let resultingState = bitwiseXor initialState

    it "assigns register x to the result of doing a bitwise xor on register x and y" $ do 
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0x9
      register `shouldBe` 0x05

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x27E