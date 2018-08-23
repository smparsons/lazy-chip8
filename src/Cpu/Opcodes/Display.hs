module Cpu.Opcodes.Display
( clearScreen,
  drawGraphics
) where

import qualified Data.Vector as V

import Data.Bits

import Cpu.Types
import Cpu.Constants
import Cpu.Helpers

--0x00E0
clearScreen :: Chip8 -> Chip8 
clearScreen chip8State = 
  chip8State { 
    graphics = V.replicate 2048 0x00, 
    drawFlag = True,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where
    originalProgramCounter = programCounter chip8State 

--0xDXYN
drawGraphics :: Chip8 -> Chip8
drawGraphics chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(0xF, collisionResult)],
    graphics = V.update originalGraphics $ V.fromList pixelChanges,
    drawFlag = True,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    originalGraphics = graphics chip8State
    originalMemory = memory chip8State
    opcode = currentOpcode chip8State
    indexRegisterValue = indexRegister chip8State
    coordinateX = (fromIntegral $ getRegisterXValue opcode originalVRegisters) :: Int
    coordinateY = (fromIntegral $ getRegisterYValue opcode originalVRegisters) :: Int
    spriteHeight = (fromIntegral $ opcode .&. 0x000F) :: Int
    spriteWidth = 8
    pixelChangesAndCollisions = 
      map
        (\(columnOffset,rowOffset) -> 
          let convertedIndexRegisterValue = fromIntegral indexRegisterValue :: Int
              index = coordinateX + columnOffset + ((coordinateY + rowOffset) * chip8NumberOfColumns)
              graphicsPixel = originalGraphics V.! index
              memoryValue = originalMemory V.! (convertedIndexRegisterValue + rowOffset)
              memoryPixel = 
                (memoryValue .&. (0x80 `shiftR` columnOffset)) `shiftR` ((spriteWidth - 1) - columnOffset)
              result = graphicsPixel `xor` memoryPixel
              collision = graphicsPixel == 1 && memoryPixel == 1
              in ((index, result), collision))
        [(columnOffset,rowOffset) | columnOffset <- [0..spriteWidth-1], rowOffset <- [0..spriteHeight-1]]
    pixelChanges = map (\((index, result), _) -> (index, result)) pixelChangesAndCollisions
    collisionOccurred = any (\((_, _), collision) -> collision) pixelChangesAndCollisions
    collisionResult = if collisionOccurred then 0x1 else 0x0