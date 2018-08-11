module Cpu.Opcodes.MemorySpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.Memory
import Cpu.Types
import Cpu.TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "setIndexRegisterToAddress" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0xA2F0,
      programCounter = 0x2AC
    }

    let resultingState = setIndexRegisterToAddress initialState

    it "sets index register to three digit address NNN" $ do
      let indexRegisterValue = indexRegister resultingState 
      indexRegisterValue `shouldBe` 0x2F0

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState 
      updatedProgramCounter `shouldBe` 0x2AE

  describe "addRegisterToIndexRegister" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0xFC1E,
      indexRegister = 0x2BF,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x5C)],
      programCounter = 0x2FD
    }

    let resultingState = addRegisterToIndexRegister initialState

    it "adds register to index register" $ do
      let indexRegisterValue = indexRegister resultingState
      indexRegisterValue `shouldBe` 0x31B

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x2FF

  describe "registerDump" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0xF755,
      indexRegister = 0x1AC,
      vRegisters = V.update originalVRegisters $ V.fromList 
        [ (0x0,0x13)
        , (0x1,0x2A)
        , (0x2,0x5C)
        , (0x3,0x4D)
        , (0x4,0x3C)
        , (0x5,0xAD)
        , (0x6,0xBC)
        , (0x7,0x54) ],
      programCounter = 0x3CC
    }

    let resultingState = registerDump initialState

    it "updates memory at address I through address I + X" $ do
      let updatedMemory = memory resultingState 
      let numberOfBytesToSlice = 8
      let updatedMemorySlice = V.toList $ V.slice 0x1AC numberOfBytesToSlice updatedMemory 
      updatedMemorySlice `shouldMatchList` [0x13,0x2A,0x5C,0x4D,0x3C,0xAD,0xBC,0x54]

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x3CE

  describe "registerLoad" $ do
    let originalMemory = memory defaultState

    let initialState = defaultState {
      currentOpcode = 0xF565,
      indexRegister = 0x20C,
      memory = V.update originalMemory $ V.fromList
        [ (0x20C,0x7A)
        , (0x20D,0xC1)
        , (0x20E,0x3B)
        , (0x20F,0x11)
        , (0x210,0x9C)
        , (0x211,0xDE) ],
      programCounter = 0x13F
    }

    let resultingState = registerLoad initialState

    it "updates registers 0 through X with values in memory at address I through I + X" $ do
      let updatedRegisters = vRegisters resultingState
      let numberOfRegistersToSlice = 6
      let updatedRegistersSlice = V.toList $ V.slice 0 numberOfRegistersToSlice updatedRegisters
      updatedRegistersSlice `shouldMatchList` [0x7A,0xC1,0x3B,0x11,0x9C,0xDE]

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState
      updatedProgramCounter `shouldBe` 0x141