module Chip8 
( emulateCpuCycle,
  initialize,
  loadFontset
) where

import System.Random
import Data.Word
import qualified Data.Vector as V

import Constants
import Cpu
import Types

emulateCpuCycle :: Chip8 -> Chip8
emulateCpuCycle = decrementSoundTimer . decrementDelayTimer . executeOpcode

initialize :: IO Chip8
initialize = do 
  newSeed <- newStdGen  
  let emptyMemory = memory chip8InitialState
  let updatedMemory = loadFontset emptyMemory
  return chip8InitialState { memory = updatedMemory, randomNumberSeed = newSeed }

loadFontset :: V.Vector Word8 -> V.Vector Word8
loadFontset givenMemory = V.update givenMemory fontsetAddresses
  where
    fontsetVector = V.fromList chip8Fontset
    fontsetAddresses = V.imap (\index fontsetByte -> (index, fontsetByte)) fontsetVector
