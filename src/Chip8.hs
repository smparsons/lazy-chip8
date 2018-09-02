module Chip8 
( emulateCpuCycle,
  initializeChip8,
  loadFontsetIntoMemory,
  loadGameIntoMemory
) where

import System.Random
import Data.Word
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Constants
import Cpu
import Types

initializeChip8 :: StdGen -> Chip8 ()
initializeChip8 newSeed = do 
  modify (\givenState -> givenState & randomNumberSeed .~ newSeed)  
  loadFontsetIntoMemory

loadFontsetIntoMemory :: Chip8 ()
loadFontsetIntoMemory = do
  let fontsetVector = V.fromList chip8Fontset
      fontsetAddresses = V.imap (\currentIndex fontsetByte -> (currentIndex, fontsetByte)) fontsetVector
      loadFontset = flip V.update fontsetAddresses
  modify (\givenState -> givenState & memory %~ loadFontset)

loadGameIntoMemory :: [Word8] -> Chip8 ()
loadGameIntoMemory game = do
  let gameVector = V.fromList $ game
      gameAddresses = V.imap (\currentIndex gameByte -> (currentIndex + 0x200, gameByte)) gameVector 
      loadGame = flip V.update gameAddresses
  modify (\givenState -> givenState & memory %~ loadGame)

emulateCpuCycle :: Chip8 ()
emulateCpuCycle = do
  executeOpcode
  decrementDelayTimer
  decrementSoundTimer