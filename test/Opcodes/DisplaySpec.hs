module Opcodes.DisplaySpec 
( spec
) where

import Test.Hspec

import Opcodes.Display
import Types
import TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "clearScreen" $ do
    let initialState = defaultState {
      currentOpcode = 0x00E0,
      graphics = V.replicate 2048 0x1,
      programCounter = 0x2D2
    }
    let resultingState = clearScreen initialState

    it "clears the screen" $ do
      let updatedGraphics = graphics resultingState
      let expectedGraphics = V.replicate 2048 0x0
      updatedGraphics `shouldBe` expectedGraphics

    it "sets draw fag to true" $ do
      let updatedDrawFlag = drawFlag resultingState
      updatedDrawFlag `shouldBe` True

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState 
      updatedProgramCounter `shouldBe` 0x2D4
  
  describe "drawGraphics" $ do
    let originalVRegisters = vRegisters defaultState
    let originalMemory = memory defaultState

    context "when drawing on clear screen" $ do
      let initialState = defaultState {
        currentOpcode = 0xD7B3,
        indexRegister = 0x3AC,
        memory = V.update originalMemory $ V.fromList
          [ (0x3AC,0x25)
          , (0x3AD,0x3C)
          , (0x3AE,0xFD) ], 
        vRegisters = V.update originalVRegisters $ V.fromList [(0x7,0x13),(0xB,0xA),(0xF,0x0)],
        programCounter = 0x2F0
      }
      let resultingState = drawGraphics initialState
      let resultingVRegisters = vRegisters resultingState 

      it "correctly updates pixel state using the xor operation" $ do 
        let updatedGraphics = graphics resultingState
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
        let updatedDrawFlag = drawFlag resultingState
        updatedDrawFlag `shouldBe` True 
        
      it "increments the program counter" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x2F2

    context "when drawing on screen with some pixel state" $ do 
      let originalGraphics = graphics defaultState

      let initialState = defaultState {
        currentOpcode = 0xDCA3,
        indexRegister = 0x2FD,
        memory = V.update originalMemory $ V.fromList
          [ (0x2FD, 0x42)
          , (0x2FE, 0xFB)
          , (0x2FF, 0x1A) ], 
        graphics = V.update originalGraphics $ V.fromList 
          [ (0x1C + (0x15 * 64), 1)
          , (0x1E + (0x15 * 64), 1)
          , (0x20 + (0x15 * 64), 1)
          , (0x1B + (0x16 * 64), 1)
          , (0x1D + (0x16 * 64), 1)
          , (0x1F + (0x16 * 64), 1)
          , (0x1A + (0x17 * 64), 1)
          , (0x1C + (0x17 * 64), 1) ],
        vRegisters = V.update originalVRegisters $ V.fromList [(0xC,0x1A),(0xA,0x15),(0xF,0x0)],
        programCounter = 0x3CA
      }
      let resultingState = drawGraphics initialState
      let resultingVRegisters = vRegisters resultingState 

      it "correctly updates pixel state using the xor operation" $ do 
        let updatedGraphics = graphics resultingState
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
        let updatedDrawFlag = drawFlag resultingState
        updatedDrawFlag `shouldBe` True 
        
      it "increments the program counter" $ do
        let updatedProgramCounter = programCounter resultingState
        updatedProgramCounter `shouldBe` 0x3CC