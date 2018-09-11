module Opcodes.Display
( clearScreen,
  drawGraphics
) where

import qualified Data.Vector as V
import Data.Bits
import Control.Monad.State
import Control.Lens

import Types
import Constants
import Helpers

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
drawGraphics :: Chip8 ()
drawGraphics = do
  chip8State <- get
  coordinateX <- fmap fromIntegral getRegisterXValue
  coordinateY <- fmap fromIntegral getRegisterYValue
  spriteHeight <- fmap fromIntegral parseOneDigitConstant
  let spriteWidth = 8
      pixelChangesAndCollisions = 
        map
          (\(colOffset,rowOffset) -> 
            let convertedIndexRegisterValue = (fromIntegral $ chip8State^.indexRegister) :: Int
                xIndex = (coordinateX + colOffset) `mod` chip8NumberOfColumns
                yIndex = (coordinateY + rowOffset) `mod` chip8NumberOfRows
                currentIndex = xIndex + (yIndex * chip8NumberOfColumns)
                graphicsPixel = (chip8State^.graphics) V.! currentIndex
                memoryValue = (chip8State^.memory) V.! (convertedIndexRegisterValue + rowOffset)
                memoryPixel = 
                  (memoryValue .&. (0x80 `shiftR` colOffset)) `shiftR` ((spriteWidth - 1) - colOffset)
                result = graphicsPixel `xor` memoryPixel
                collision = graphicsPixel == 1 && memoryPixel == 1
                in ((currentIndex, result), collision))
          [(colOffset,rowOffset) | colOffset <- [0..spriteWidth-1], rowOffset <- [0..spriteHeight-1]]
      pixelChanges = map (\((currentIndex, result), _) -> (currentIndex, result)) pixelChangesAndCollisions
      collisionResult = if (any (\((_, _), collision) -> collision) pixelChangesAndCollisions) then 0x1 else 0x0
      updateGraphics = flip V.update $ V.fromList pixelChanges
      storeCollision = flip V.update $ V.fromList [(0xF, collisionResult)]
  modify (\givenState -> givenState & graphics %~ updateGraphics)
  modify (\givenState -> givenState & vRegisters %~ storeCollision)
  modify (\givenState -> givenState & drawFlag .~ True)
  incrementProgramCounter