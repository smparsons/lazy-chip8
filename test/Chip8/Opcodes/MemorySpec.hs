module Chip8.Opcodes.MemorySpec 
( spec
) where

import Test.Hspec

import Chip8.Opcodes.Memory
import Chip8
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "setIndexRegisterToAddress" $ do
    let initialState = chip8InitialState {
      _currentOpcode = 0xA2F0,
      _programCounter = 0x2AC
    }

    let resultingState = execState setIndexRegisterToAddress initialState

    it "sets index register to three digit address NNN" $ do
      let indexRegisterValue = resultingState^.indexRegister
      indexRegisterValue `shouldBe` 0x2F0

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x2AE

  describe "addRegisterToIndexRegister" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xFC1E,
      _indexRegister = 0x2BF,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x5C)],
      _programCounter = 0x2FD
    }

    let resultingState = execState addRegisterToIndexRegister initialState

    it "adds register to index register" $ do
      let indexRegisterValue = resultingState^.indexRegister
      indexRegisterValue `shouldBe` 0x31B

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x2FF

  describe "registerDump" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xF755,
      _indexRegister = 0x1AC,
      _vRegisters = V.update originalVRegisters $ V.fromList 
        [ (0x0,0x13)
        , (0x1,0x2A)
        , (0x2,0x5C)
        , (0x3,0x4D)
        , (0x4,0x3C)
        , (0x5,0xAD)
        , (0x6,0xBC)
        , (0x7,0x54) ],
      _programCounter = 0x3CC
    }

    let resultingState = execState registerDump initialState

    it "updates memory at address I through address I + X" $ do
      let updatedMemory = resultingState^.memory
      let numberOfBytesToSlice = 8
      let updatedMemorySlice = V.toList $ V.slice 0x1AC numberOfBytesToSlice updatedMemory 
      updatedMemorySlice `shouldMatchList` [0x13,0x2A,0x5C,0x4D,0x3C,0xAD,0xBC,0x54]

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x3CE

  describe "registerLoad" $ do
    let originalMemory = chip8InitialState^.memory

    let initialState = chip8InitialState {
      _currentOpcode = 0xF565,
      _indexRegister = 0x20C,
      _memory = V.update originalMemory $ V.fromList
        [ (0x20C,0x7A)
        , (0x20D,0xC1)
        , (0x20E,0x3B)
        , (0x20F,0x11)
        , (0x210,0x9C)
        , (0x211,0xDE) ],
      _programCounter = 0x13F
    }

    let resultingState = execState registerLoad initialState

    it "updates registers 0 through X with values in memory at address I through I + X" $ do
      let updatedRegisters = resultingState^.vRegisters
      let numberOfRegistersToSlice = 6
      let updatedRegistersSlice = V.toList $ V.slice 0 numberOfRegistersToSlice updatedRegisters
      updatedRegistersSlice `shouldMatchList` [0x7A,0xC1,0x3B,0x11,0x9C,0xDE]

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x141

  describe "storeBCD" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xF733,
      _indexRegister = 0x17B,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0x7,0xAF)],
      _programCounter = 0x232
    }

    let resultingState = execState storeBCD initialState

    it "updates memory at location I, I + 1, and I + 2 with the BCD representation of register X" $ do
      let updatedMemory = resultingState^.memory
      let numberOfBytesToSlice = 3
      let updatedMemorySlice = V.toList $ V.slice 0x17B numberOfBytesToSlice updatedMemory
      updatedMemorySlice `shouldMatchList` [0x1,0x7,0x5]

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x234

  describe "storeSpriteLocation" $ do
    let originalVRegisters = chip8InitialState^.vRegisters

    let initialState = chip8InitialState {
      _currentOpcode = 0xF529,
      _indexRegister = 0x213,
      _vRegisters = V.update originalVRegisters $ V.fromList [(0x5,0xA)],
      _programCounter = 0x240
    }

    let resultingState = execState storeSpriteLocation initialState

    it "updates I to the location of the 4x5 font representation of the character in X" $ do
      let updatedIndexRegisterValue = resultingState^.indexRegister
      updatedIndexRegisterValue `shouldBe` 0x32

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x242