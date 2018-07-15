module Cpu
( Chip8(..),
  clearScreen,
  returnFromSubroutine,
  jumpToAddress,
  callSubroutine,
  addTwoRegisterValues
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

clearScreen :: Chip8 -> Chip8 
clearScreen chip8State = chip8State { graphics = V.replicate 2048 0x00 }

returnFromSubroutine :: Chip8 -> Chip8
returnFromSubroutine chip8State =
  chip8State {
    stack = V.init originalStack, 
    stackPointer = originalStackPointer - 1,
    programCounter = lastAddress + 2
  }
  where 
    originalStack = stack chip8State
    originalStackPointer = stackPointer chip8State
    lastAddress = V.last originalStack

jumpToAddress :: Chip8 -> Chip8 
jumpToAddress chip8State = 
  chip8State { 
    programCounter = newAddress
  }
  where 
    opcode = currentOpcode chip8State
    newAddress = opcode .&. 0x0FFF

callSubroutine :: Chip8 -> Chip8 
callSubroutine chip8State = 
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
    
addTwoRegisterValues :: Chip8 -> Chip8
addTwoRegisterValues chip8State = 
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

    