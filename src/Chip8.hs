module Chip8 
( emulateCpuCycle
) where

import Cpu
import Types

emulateCpuCycle :: Chip8 -> Chip8
emulateCpuCycle = decrementSoundTimer . decrementDelayTimer . executeOpcode

