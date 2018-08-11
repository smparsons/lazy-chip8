module Cpu.TestHelpers 
( defaultState
) where

import qualified Data.Vector as V

import Cpu.Types

defaultState :: Chip8
defaultState = Chip8 {
  currentOpcode = 0x0000,
  memory = V.replicate 4096 0x00,
  vRegisters = V.replicate 16 0x00,
  indexRegister = 0x0000,
  programCounter = 0x0000,
  graphics = V.replicate 2048 0x00,
  delayTimer = 0x00,
  soundTimer = 0x00,
  stack = V.empty,
  stackPointer = 0x0000,
  keyState = V.empty
}