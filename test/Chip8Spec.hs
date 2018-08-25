module Chip8Spec 
( spec
) where

import Test.Hspec
import Chip8
import Types
import Constants

import Data.Word
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "loadFontset" $ do
    it "correctly loads the font set" $ do
      let initialMemory = memory chip8InitialState
      let updatedMemory = loadFontset initialMemory

      let numberOfBytesToSlice = 0x50
      let fontset = V.toList $ V.slice 0 numberOfBytesToSlice updatedMemory 
      fontset `shouldMatchList` chip8Fontset