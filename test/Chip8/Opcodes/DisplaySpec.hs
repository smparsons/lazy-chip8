module Chip8.Opcodes.DisplaySpec 
( spec
) where

import Test.Hspec

import Chip8.Opcodes.Display
import Chip8
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "clearScreen" $ do
    let initialState = chip8InitialState {
      _currentOpcode = 0x00E0,
      _graphics = V.replicate 2048 0x1,
      _programCounter = 0x2D2
    }
    let resultingState = execState clearScreen initialState

    it "clears the screen" $ do
      let updatedGraphics = resultingState^.graphics
      let expectedGraphics = V.replicate 2048 0x0
      updatedGraphics `shouldBe` expectedGraphics

    it "sets draw fag to true" $ do
      let updatedDrawFlag = resultingState^.drawFlag
      updatedDrawFlag `shouldBe` True

    it "increments the program counter" $ do
      let updatedProgramCounter = resultingState^.programCounter
      updatedProgramCounter `shouldBe` 0x2D4
  
  describe "drawGraphics" $ do
    let originalVRegisters = chip8InitialState^.vRegisters
    let originalMemory = chip8InitialState^.memory

    context "when drawing on clear screen" $ do
      let initialState = chip8InitialState {
        _currentOpcode = 0xD7B3,
        _indexRegister = 0x3AC,
        _memory = V.update originalMemory $ V.fromList
          [ (0x3AC,0x25)
          , (0x3AD,0x3C)
          , (0x3AE,0xFD) ], 
        _vRegisters = V.update originalVRegisters $ V.fromList [(0x7,0x13),(0xB,0xA),(0xF,0x0)],
        _programCounter = 0x2F0
      }
      let resultingState = execState drawGraphics initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "correctly updates pixel state using the xor operation" $ do 
        let updatedGraphics = resultingState^.graphics
        let numberOfBytesToSlice = 8

        let firstRowGraphicSlice = V.toList $ V.slice (0x13 + (0xA * 64)) numberOfBytesToSlice updatedGraphics 
        firstRowGraphicSlice `shouldMatchList` [0,0,1,0,0,1,0,1]

        let secondRowGraphicSlice = V.toList $ V.slice (0x13 + (0xB * 64)) numberOfBytesToSlice updatedGraphics
        secondRowGraphicSlice `shouldMatchList` [0,0,1,1,1,1,0,0]

        let thirdRowGraphicSlice = V.toList $ V.slice (0x13 + (0xC * 64)) numberOfBytesToSlice updatedGraphics
        thirdRowGraphicSlice `shouldMatchList` [1,1,1,1,1,1,0,1]

      it "sets carry register to 0" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x0

      it "sets draw flag to true" $ do
        let updatedDrawFlag = resultingState^.drawFlag
        updatedDrawFlag `shouldBe` True 
        
      it "increments the program counter" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x2F2

    context "when drawing on screen with some pixel state" $ do 
      let originalGraphics = chip8InitialState^.graphics

      let initialState = chip8InitialState {
        _currentOpcode = 0xDCA3,
        _indexRegister = 0x2FD,
        _memory = V.update originalMemory $ V.fromList
          [ (0x2FD, 0x42)
          , (0x2FE, 0xFB)
          , (0x2FF, 0x1A) ], 
        _graphics = V.update originalGraphics $ V.fromList 
          [ (0x1C + (0x15 * 64), 1)
          , (0x1E + (0x15 * 64), 1)
          , (0x20 + (0x15 * 64), 1)
          , (0x1B + (0x16 * 64), 1)
          , (0x1D + (0x16 * 64), 1)
          , (0x1F + (0x16 * 64), 1)
          , (0x1A + (0x17 * 64), 1)
          , (0x1C + (0x17 * 64), 1) ],
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x1A),(0xA,0x15),(0xF,0x0)],
        _programCounter = 0x3CA
      }
      let resultingState = execState drawGraphics initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "correctly updates pixel state using the xor operation" $ do 
        let updatedGraphics = resultingState^.graphics
        let numberOfBytesToSlice = 8

        let firstRowGraphicSlice = V.toList $ V.slice (0x1A + (0x15 * 64)) numberOfBytesToSlice updatedGraphics 
        firstRowGraphicSlice `shouldMatchList` [0,1,1,0,1,0,0,0]

        let secondRowGraphicSlice = V.toList $ V.slice (0x1A + (0x16 * 64)) numberOfBytesToSlice updatedGraphics
        secondRowGraphicSlice `shouldMatchList` [1,0,1,0,1,1,1,1]

        let thirdRowGraphicSlice = V.toList $ V.slice (0x1A + (0x17 * 64)) numberOfBytesToSlice updatedGraphics
        thirdRowGraphicSlice `shouldMatchList` [1,0,1,1,1,0,1,0]

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "sets draw flag to true" $ do
        let updatedDrawFlag = resultingState^.drawFlag
        updatedDrawFlag `shouldBe` True 
        
      it "increments the program counter" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x3CC

    context "when drawing on screen past the screen boundary" $ do 
      let originalGraphics = chip8InitialState^.graphics

      let initialState = chip8InitialState {
        _currentOpcode = 0xDB53,
        _indexRegister = 0x31B,
        _memory = V.update originalMemory $ V.fromList
          [ (0x31B, 0x25)
          , (0x31C, 0xA3)
          , (0x31D, 0x7C) ], 
        _graphics = V.update originalGraphics $ V.fromList 
          [ (0x28 + (0x1E * 64), 1)
          , (0x2A + (0x1E * 64), 1)
          , (0x2C + (0x1E * 64), 1)
          , (0x29 + (0x1F * 64), 1)
          , (0x2B + (0x1F * 64), 1)
          , (0x28, 1)
          , (0x2A, 1) ],
        _vRegisters = V.update originalVRegisters $ V.fromList [(0xB,0x28),(0x5,0x1E),(0xF,0x0)],
        _programCounter = 0x29B
      }
      let resultingState = execState drawGraphics initialState
      let resultingVRegisters = resultingState^.vRegisters

      it "correctly updates pixel state using the xor operation, and wraps to the other side of the screen" $ do 
        let updatedGraphics = resultingState^.graphics
        let numberOfBytesToSlice = 8

        let firstRowGraphicSlice = V.toList $ V.slice (0x28 + (0x1E * 64)) numberOfBytesToSlice updatedGraphics 
        firstRowGraphicSlice `shouldMatchList` [1,0,0,0,1,1,0,1]

        let secondRowGraphicSlice = V.toList $ V.slice (0x28 + (0x1F * 64)) numberOfBytesToSlice updatedGraphics
        secondRowGraphicSlice `shouldMatchList` [1,1,1,1,0,0,1,1]

        let thirdRowGraphicSlice = V.toList $ V.slice 0x28 numberOfBytesToSlice updatedGraphics
        thirdRowGraphicSlice `shouldMatchList` [1,1,0,1,1,1,0,0]

      it "sets carry register to 1" $ do
        let carry = resultingVRegisters V.! 0xF
        carry `shouldBe` 0x1

      it "sets draw flag to true" $ do
        let updatedDrawFlag = resultingState^.drawFlag
        updatedDrawFlag `shouldBe` True 
        
      it "increments the program counter" $ do
        let updatedProgramCounter = resultingState^.programCounter
        updatedProgramCounter `shouldBe` 0x29D