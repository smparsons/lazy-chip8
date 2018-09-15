module Chip8.Helpers
( parseRegisterXNumber,
  getRegisterXValue,
  parseRegisterYNumber,
  getRegisterYValue,
  parseOneDigitConstant,
  parseTwoDigitConstant,
  parseThreeDigitConstant,
  incrementProgramCounter,
  skipNextInstruction,
  skipNextInstructionIf
) where

import Data.Word
import Data.Bits
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Chip8.Types
import Chip8.Constants

parseRegisterXNumber :: Chip8 Int 
parseRegisterXNumber = 
  gets (\givenState -> (fromIntegral $ (givenState^.currentOpcode .&. 0x0F00) `shiftR` 8) :: Int)

getRegisterXValue :: Chip8 Word8
getRegisterXValue = do
  chip8State <- get
  registerNumber <- parseRegisterXNumber  
  return $ (chip8State^.vRegisters) V.! registerNumber

parseRegisterYNumber :: Chip8 Int
parseRegisterYNumber = 
  gets (\givenState -> (fromIntegral $ (givenState^.currentOpcode .&. 0x00F0) `shiftR` 4) :: Int)

getRegisterYValue :: Chip8 Word8
getRegisterYValue = do
  chip8State <- get
  registerNumber <- parseRegisterYNumber 
  return $ (chip8State^.vRegisters) V.! registerNumber 

parseOneDigitConstant :: Chip8 Word8
parseOneDigitConstant = gets (\givenState -> (fromIntegral $ (givenState^.currentOpcode) .&. 0x000F) :: Word8)

parseTwoDigitConstant :: Chip8 Word8
parseTwoDigitConstant = gets (\givenState -> (fromIntegral $ (givenState^.currentOpcode) .&. 0x00FF) :: Word8)

parseThreeDigitConstant :: Chip8 Word16
parseThreeDigitConstant = gets (\givenState -> (givenState^.currentOpcode) .&. 0x0FFF)

incrementProgramCounter :: Chip8 ()
incrementProgramCounter = modify (\givenState -> givenState & programCounter +~ programCounterIncrement)

skipNextInstruction :: Chip8 ()
skipNextInstruction = modify (\givenState -> givenState & programCounter +~ (programCounterIncrement * 2))

skipNextInstructionIf :: Bool -> Chip8 ()
skipNextInstructionIf booleanValue = if booleanValue then skipNextInstruction else incrementProgramCounter