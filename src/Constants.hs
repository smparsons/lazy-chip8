module Constants
( programCounterIncrement,
  chip8NumberOfColumns,
  chip8InitialState
) where

import System.Random
import qualified Data.Vector as V
import Data.Word

import Types

programCounterIncrement :: Word16
programCounterIncrement = 0x0002

chip8NumberOfColumns :: Int
chip8NumberOfColumns = 64

chip8InitialState :: Chip8
chip8InitialState = Chip8 {
  currentOpcode = 0x0000,
  memory = V.replicate 4096 0x00,
  vRegisters = V.replicate 16 0x00,
  indexRegister = 0x0000,
  programCounter = 0x200,
  graphics = V.replicate 2048 0x00,
  delayTimer = 0x00,
  soundTimer = 0x00,
  stack = V.replicate 16 0x00,
  stackPointer = 0x0000,
  keyState = V.replicate 16 0x00,
  drawFlag = False,
  randomNumberSeed = mkStdGen 0
}