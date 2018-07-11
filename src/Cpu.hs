module Cpu
( Chip8(..),
  executeOpcode2NNN 
) where

import Data.Word
import Data.Bits

data Chip8 = Chip8 {
  currentOpcode :: Word16,
  memory :: [Word8],
  vRegisters :: [Word8],
  indexRegister :: Word16,
  programCounter :: Word16,
  graphics :: [Word8],
  delayTimer :: Word8,
  soundTimer :: Word8,
  stack :: [Word16],
  stackPointer :: Word16,
  keyState :: [Word8]
} deriving (Show, Eq)

executeOpcode2NNN :: Chip8 -> Chip8 
executeOpcode2NNN chip8State = 
  chip8State { 
    stack = originalStack ++ [originalProgramCounter], 
    stackPointer = originalStackPointer + 1, 
    programCounter = (.&.) originalOpcode 0x0FFF
  }
  where 
    originalOpcode = currentOpcode chip8State
    originalStack = stack chip8State 
    originalStackPointer = stackPointer chip8State 
    originalProgramCounter = programCounter chip8State 
    