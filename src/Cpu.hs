module Cpu
( Chip8(..),
  executeOpcode00E0,
  executeOpcode00EE,
  executeOpcode1NNN,
  executeOpcode2NNN,
  executeOpcode8XY4
) where

import Data.Word
import Data.Bits
import qualified Data.Vector as V

data Chip8 = Chip8 {
  currentOpcode :: Word16,
  memory :: V.Vector Word8,
  vRegisters :: V.Vector Word8,
  indexRegister :: Word16,
  programCounter :: Word16,
  graphics :: V.Vector Word8,
  delayTimer :: Word8,
  soundTimer :: Word8,
  stack :: V.Vector Word16,
  stackPointer :: Word16,
  keyState :: V.Vector Word8
} deriving (Show, Eq)

executeOpcode00E0 :: Chip8 -> Chip8 
executeOpcode00E0 chip8State = chip8State { graphics = V.replicate 2048 0x00 }

executeOpcode00EE :: Chip8 -> Chip8
executeOpcode00EE chip8State =
  chip8State {
    stack = V.init originalStack, 
    stackPointer = originalStackPointer - 1,
    programCounter = lastAddress + 2
  }
  where 
    originalStack = stack chip8State
    originalStackPointer = stackPointer chip8State
    lastAddress = V.last originalStack

executeOpcode1NNN :: Chip8 -> Chip8 
executeOpcode1NNN chip8State = 
  chip8State { 
    programCounter = newAddress
  }
  where 
    opcode = currentOpcode chip8State
    newAddress = opcode .&. 0x0FFF

executeOpcode2NNN :: Chip8 -> Chip8 
executeOpcode2NNN chip8State = 
  chip8State { 
    stack = V.snoc originalStack originalProgramCounter, 
    stackPointer = originalStackPointer + 1, 
    programCounter = opcode .&. 0x0FFF
  }
  where 
    opcode = currentOpcode chip8State
    originalStack = stack chip8State 
    originalStackPointer = stackPointer chip8State 
    originalProgramCounter = programCounter chip8State 
    
executeOpcode8XY4 :: Chip8 -> Chip8
executeOpcode8XY4 chip8State = 
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,total),(0xF,carry)],
    programCounter = originalProgramCounter + 2
  }
  where
    originalVRegisters = vRegisters chip8State 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerX = (fromIntegral $ shiftR (opcode .&. 0x0F00) 8) :: Int
    registerY = (fromIntegral $ shiftR (opcode .&. 0x00F0) 4) :: Int
    registerXValue = originalVRegisters V.! registerX
    registerYValue = originalVRegisters V.! registerY
    total = registerXValue + registerYValue
    carry = if registerYValue > (0xFF - registerXValue) then 0x1 else 0x0

    