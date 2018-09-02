module Chip8Spec 
( spec
) where

import Test.Hspec
import Chip8
import Types
import Constants

import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

spec :: Spec
spec = do
  describe "loadFontsetIntoMemory" $ do
    it "correctly loads fontset into memory at address 0x0" $ do
      let resultingState = execState loadFontsetIntoMemory chip8InitialState
      let resultingMemory = resultingState^.memory

      let numberOfBytesToSlice = 0x50
      let fontset = V.toList $ V.slice 0 numberOfBytesToSlice resultingMemory 
      fontset `shouldMatchList` chip8Fontset

  describe "loadGameIntoMemory" $ do
    it "correctly loads game into memory at address 0x200" $ do
      let mazeGame = 
            [ 0xA2, 0x1E, 0xC2, 0x01, 0x32, 0x01, 0xA2, 0x1A
            , 0xD0, 0x14, 0x70, 0x04, 0x30, 0x40, 0x12, 0x00
            , 0x60, 0x00, 0x71, 0x04, 0x31, 0x20, 0x12, 0x00 
            , 0x12, 0x18, 0x80, 0x40, 0x20, 0x10, 0x20, 0x40
            , 0x80, 0x10 ]

      let resultingState = execState (loadGameIntoMemory mazeGame) chip8InitialState
      let resultingMemory = resultingState^.memory

      let numberOfBytesToSlice = 0x22
      let loadedGame = V.toList $ V.slice 0x200 numberOfBytesToSlice resultingMemory
      loadedGame `shouldMatchList` mazeGame 
