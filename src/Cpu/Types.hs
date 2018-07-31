module Cpu.Types
( Chip8(..)
) where

import Data.Word
import qualified Data.Vector as V

data Chip8 = Chip8 {
  currentOpcode :: Word16,
  memory :: V.Vector Word8,
  vRegisters :: V.Vector Word8,
  indexRegister :: Word16,
  programCounter :: Word16,
  graphics :: V.Vector Word8,
  delayTimer :: Word8,
  soundTimer :: Word8,
  stack :: V.Vector Word16,
  stackPointer :: Word16,
  keyState :: V.Vector Word8
} deriving (Show, Eq)