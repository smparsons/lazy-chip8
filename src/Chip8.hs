module Chip8 
( emulateCpuCycle,
  initializeChip8,
  loadFontset,
  loadGameByFilePath,
  loadGameIntoMemory
) where

import System.Random
import Data.Word
import Data.ByteString as BS
import qualified Data.Vector as V

import Constants
import Cpu
import Types

emulateCpuCycle :: Chip8 -> Chip8
emulateCpuCycle = decrementSoundTimer . decrementDelayTimer . executeOpcode

initializeChip8 :: IO Chip8
initializeChip8 = do 
  newSeed <- newStdGen  
  let emptyMemory = memory chip8InitialState
  let updatedMemory = loadFontset emptyMemory
  return chip8InitialState { memory = updatedMemory, randomNumberSeed = newSeed }

loadFontset :: V.Vector Word8 -> V.Vector Word8
loadFontset givenMemory = V.update givenMemory fontsetAddresses
  where
    fontsetVector = V.fromList chip8Fontset
    fontsetAddresses = V.imap (\currentIndex fontsetByte -> (currentIndex, fontsetByte)) fontsetVector

loadGameByFilePath :: String -> Chip8 -> IO Chip8
loadGameByFilePath filePath chip8State = do
  contents <- BS.readFile filePath
  let game = unpack contents
  let initialMemory = memory chip8State
  let updatedMemory = loadGameIntoMemory initialMemory game
  return chip8State { memory = updatedMemory }

loadGameIntoMemory :: V.Vector Word8 -> [Word8] -> V.Vector Word8
loadGameIntoMemory givenMemory game = V.update givenMemory gameAddresses
  where 
    gameVector = V.fromList game
    gameAddresses = V.imap (\currentIndex gameByte -> (currentIndex + 0x200, gameByte)) gameVector


