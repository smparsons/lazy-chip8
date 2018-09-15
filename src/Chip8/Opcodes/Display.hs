module Chip8.Opcodes.Display
  ( clearScreen
  , drawGraphics
  ) where

import Chip8.Constants
import Chip8.Helpers
import Chip8.Types
import Control.Monad.State
import Control.Lens
import Data.Bits
import qualified Data.Vector as V
import Data.Word


{-
  0x00E0
  Clears the screen.
-}
clearScreen :: Chip8 ()
clearScreen = do
  modify (\givenState -> givenState & graphics .~ (V.replicate 2048 0x00))
  modify (\givenState -> givenState & drawFlag .~ True)
  incrementProgramCounter 

{-
  0xDXYN
  Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels. 
  Each row of 8 pixels is read as bit-coded starting from memory location I; I value doesn’t 
  change after the execution of this instruction. As described above, VF is set to 1 if any 
  screen pixels are flipped from set to unset when the sprite is drawn, and to 0 if that 
  doesn’t happen
-}
type PixelUpdate = (Int, Word8)
type PixelCoordinates = (Int, Int)
type Offsets = (Int, Int)

data PixelUpdates = PixelUpdates {
  updates :: [PixelUpdate],
  collisionResult :: Word8
}

drawGraphics :: Chip8 ()
drawGraphics = do
  result <- getPixelsToUpdate
  let updateGraphics = flip V.update $ V.fromList $ updates result
      storeCollision = flip V.update $ V.fromList [(0xF, collisionResult result)]
  modify (\givenState -> givenState & graphics %~ updateGraphics)
  modify (\givenState -> givenState & vRegisters %~ storeCollision)
  modify (\givenState -> givenState & drawFlag .~ True)
  incrementProgramCounter

getPixelsToUpdate :: Chip8 PixelUpdates
getPixelsToUpdate = do
  coordinateX <- fmap fromIntegral getRegisterXValue
  coordinateY <- fmap fromIntegral getRegisterYValue
  spriteHeight <- fmap fromIntegral parseOneDigitConstant
  let offsets = [(colOffset,rowOffset) | colOffset <- [0..chip8SpriteWidth-1], rowOffset <- [0..spriteHeight-1]]
  pixelUpdateResults <- mapM (getPixelUpdate (coordinateX, coordinateY)) offsets
  return PixelUpdates { 
    updates = map (\(pixelUpdate, _) -> pixelUpdate) pixelUpdateResults,
    collisionResult = if (any (\(_, collision) -> collision) pixelUpdateResults) then 0x1 else 0x0
  }    

getPixelUpdate :: PixelCoordinates -> Offsets -> Chip8 (PixelUpdate, Bool)
getPixelUpdate (coordinateX, coordinateY) (colOffset, rowOffset) = do
  chip8State <- get
  let spriteWidth = 8
      convertedIndexRegisterValue = (fromIntegral $ chip8State^.indexRegister) :: Int
      xIndex = (coordinateX + colOffset) `mod` chip8NumberOfColumns
      yIndex = (coordinateY + rowOffset) `mod` chip8NumberOfRows
      currentIndex = xIndex + (yIndex * chip8NumberOfColumns)
      graphicsPixel = (chip8State^.graphics) V.! currentIndex
      memoryValue = (chip8State^.memory) V.! (convertedIndexRegisterValue + rowOffset)
      memoryPixel = (memoryValue .&. (0x80 `shiftR` colOffset)) `shiftR` ((spriteWidth - 1) - colOffset)
      result = graphicsPixel `xor` memoryPixel
      collision = graphicsPixel == 1 && memoryPixel == 1
  return ((currentIndex, result), collision)