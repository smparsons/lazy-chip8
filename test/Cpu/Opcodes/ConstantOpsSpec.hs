module Cpu.Opcodes.ConstantOpsSpec 
( spec
) where

import Test.Hspec

import Cpu.Opcodes.ConstantOps
import Cpu.Types
import Cpu.TestHelpers
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "setRegisterToConstant" $ do
    let originalVRegisters = vRegisters defaultState

    let initialState = defaultState {
      currentOpcode = 0x6C23,
      vRegisters = V.update originalVRegisters $ V.fromList [(0xC, 0x5A)],
      programCounter = 0x180
    }

    let resultingState = setRegisterToConstant initialState

    it "sets register to constant" $ do
      let resultingVRegisters = vRegisters resultingState
      let register = resultingVRegisters V.! 0xC
      register `shouldBe` 0x23

    it "increments the program counter" $ do
      let updatedProgramCounter = programCounter resultingState 
      updatedProgramCounter `shouldBe` 0x182