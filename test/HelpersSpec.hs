module HelpersSpec 
( spec
) where

import Test.Hspec
import Helpers
import Data.Word
import qualified Data.Vector as V

emptyVRegisters :: V.Vector Word8
emptyVRegisters = V.replicate 16 0x00

spec :: Spec
spec = do
  describe "parseRegisterXNumber" $ do
    it "parses X out of the opcode" $ do
      let registerXNumber = parseRegisterXNumber 0x28A0
      registerXNumber `shouldBe` 0x8

  describe "parseRegisterYNumber" $ do
    it "parses Y out of the opcode" $ do
      let registerYNumber = parseRegisterYNumber 0x13CF
      registerYNumber `shouldBe` 0xC

  describe "getRegisterXValue" $ do
    let initialOpcode = 0x5AC0
    let initialVRegisters = V.update emptyVRegisters $ V.fromList [(0xA,0x3C)]

    it "returns the value stored in register X" $ do
      let registerXValue = getRegisterXValue initialOpcode initialVRegisters 
      registerXValue `shouldBe` 0x3C

  describe "getRegisterYValue" $ do
    let initialOpcode = 0x3720
    let initialVRegisters = V.update emptyVRegisters $ V.fromList [(0x2,0x1F)]

    it "returns the value stored in register Y" $ do
      let registerYValue = getRegisterYValue initialOpcode initialVRegisters 
      registerYValue `shouldBe` 0x1F

  describe "parseTwoDigitConstant" $ do
    let initialOpcode = 0x5A27
    
    it "returns the last two digits from the opcode" $ do
      let constant = parseTwoDigitConstant initialOpcode
      constant `shouldBe` 0x27

  describe "parseThreeDigitConstant" $ do
    let initialOpcode = 0x4AC3

    it "returns the last three digits from the opcode" $ do
      let constant = parseThreeDigitConstant initialOpcode 
      constant `shouldBe` 0xAC3