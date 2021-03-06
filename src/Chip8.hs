module Chip8 
  ( emulateCpuCycle
  , initializeChip8
  , loadFontsetIntoMemory
  , loadGameIntoMemory
  , getGraphicsAsByteString
  , storeKeyPressChanges
  , module Chip8.Constants
  , module Chip8.Types
  ) where

import Chip8.Constants
import Chip8.Cpu
import Chip8.Types
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Word
import System.Random

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

getGraphicsAsByteString :: Chip8 BS.ByteString
getGraphicsAsByteString = do
  chip8Graphics <- gets (\givenState -> V.toList $ givenState^.graphics)
  let black = [0, 0, 0, 0] :: [Word8]
      white = [255, 255, 255, 255] :: [Word8]
      flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []
      rgbaFormatGraphics = flatten $ map (\pixelState -> if pixelState == 1 then white else black) chip8Graphics
      graphicsByteString = BS.pack rgbaFormatGraphics
  return graphicsByteString

storeKeyPressChanges :: [(Int, KeyPressState)] -> Chip8 ()
storeKeyPressChanges keyPressChanges = do
  let loadKeyPressChanges = flip V.update $ V.fromList keyPressChanges
  modify (\givenState -> givenState & keyState %~ loadKeyPressChanges)
