module Cpu.Opcodes.KeyOps
( keyIsPressed,
  keyIsNotPressed,
  awaitKeyPress
) where

import Data.Word
import qualified Data.Vector as V

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

{-
  0xEX9E
  Skips the next instruction if the key stored in VX is pressed. (Usually the next instruction 
  is a jump to skip a code block)
-}
keyIsPressed :: Chip8 -> Chip8 
keyIsPressed chip8State =
  chip8State {
    programCounter = if keyValue == 0x1 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State
    originalKeyState = keyState chip8State  
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    key = (fromIntegral registerXValue) :: Int
    keyValue = originalKeyState V.! key

{-
  0xEXA1
  Skips the next instruction if the key stored in VX isn't pressed. (Usually the next instruction 
  is a jump to skip a code block)
-}
keyIsNotPressed :: Chip8 -> Chip8
keyIsNotPressed chip8State = 
  chip8State {
    programCounter = if keyValue == 0x0 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State
    originalKeyState = keyState chip8State  
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    key = (fromIntegral registerXValue) :: Int
    keyValue = originalKeyState V.! key

{-
  0xFX0A
  A key press is awaited, and then stored in VX. (Blocking Operation. All instruction halted 
  until next key event)
-}
awaitKeyPress :: Chip8 -> Chip8
awaitKeyPress chip8State =
  case pressedKey of
    Nothing -> chip8State
    Just key -> 
      let convertedKey = fromIntegral key :: Word8 
      in chip8State { 
        vRegisters = V.update originalVRegisters $ V.fromList [(registerX,convertedKey)],
        programCounter = originalProgramCounter + programCounterIncrement
      }
  where
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State
    originalKeyState = keyState chip8State 
    opcode = currentOpcode chip8State
    registerX = parseRegisterXNumber opcode
    pressedKey = V.findIndex (\key -> key == 0x1) originalKeyState