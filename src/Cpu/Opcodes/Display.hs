module Cpu.Opcodes.Display
( clearScreen
) where

import qualified Data.Vector as V

import Cpu.Types

--0x00E0
clearScreen :: Chip8 -> Chip8 
clearScreen chip8State = chip8State { graphics = V.replicate 2048 0x00 }