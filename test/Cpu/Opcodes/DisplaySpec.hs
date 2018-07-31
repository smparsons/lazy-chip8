module Cpu.Opcodes.DisplaySpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.Display
import Cpu.Types
import Cpu.TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "clearScreen" $ do
    let initialState = defaultState {
      graphics = V.replicate 2048 0x1
    }
    let resultingState = clearScreen initialState

    it "clears the screen" $ do
      let updatedGraphics = graphics resultingState
      let expectedGraphics = V.replicate 2048 0x0
      updatedGraphics `shouldBe` expectedGraphics