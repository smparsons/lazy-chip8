module Opcodes.BitwiseOpsSpec 
( spec
) where

import Test.Hspec

import Opcodes.BitwiseOps
import Types
import TestHelpers
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

  describe "randomBitwiseAnd" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0xC572,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x5,0xAB)],
      programCounter = 0x320
    }

    let resultingState = randomBitwiseAnd initialState

    it "assigns register x to the result of bitwise and on random number and two digit constant" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0x5
      register `shouldBe` 0x32

    it "generates a new seed" $ do
      let newSeed = randomNumberSeed resultingState
      let newSeedString = show newSeed
      newSeedString `shouldBe` "444635568 40692"

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x322

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

  describe "shiftRight" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x8746,
      vRegisters = V.update originalVRegisters $ V.fromList [(0x7,0x3F),(0x4,0x1C),(0xF,0x1)],
      programCounter = 0x223
    }

    let resultingState = shiftRight initialState
    let resultingVRegisters = vRegisters resultingState 

    it "assigns register x to the result of doing a right bit shift on register y's value" $ do
      let register = resultingVRegisters V.! 0x7
      register `shouldBe` 0x0E

    it "assigns the least significant bit to the carry register before the bit shift" $ do
      let register = resultingVRegisters V.! 0xF
      register `shouldBe` 0x0

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x225

  describe "shiftLeft" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x8C1E,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x2F),(0x1,0xEA),(0xF,0x0)],
      programCounter = 0x11A
    }

    let resultingState = shiftLeft initialState
    let resultingVRegisters = vRegisters resultingState

    it "assigns register x and y to the result of doing a left bit shift on register y's value" $ do
      let registerX = resultingVRegisters V.! 0xC
      let registerY = resultingVRegisters V.! 0x1
      registerX `shouldBe` 0xD4
      registerY `shouldBe` 0xD4

    it "assigns the most significant bit to the carry register before the bit shift" $ do
      let register = resultingVRegisters V.! 0xF
      register `shouldBe` 0x1

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x11C