module Chip8 
( emulateCpuCycle,
  initialize
) where

import Data.Word
import qualified Data.Vector as V

import Constants
import Cpu
import Types

emulateCpuCycle :: Chip8 -> Chip8
emulateCpuCycle = decrementSoundTimer . decrementDelayTimer . executeOpcode

initialize :: Chip8
initialize = chip8InitialState { memory = loadFontset originalMemory }
  where 
    originalMemory = memory chip8InitialState

loadFontset :: V.Vector Word8 -> V.Vector Word8
loadFontset = undefined