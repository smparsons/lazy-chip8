module Cpu.Opcodes
( Chip8(..),
  clearScreen,
  returnFromSubroutine,
  jumpToAddress,
  callSubroutine,
  registerEqualsConstant,
  registerDoesNotEqualConstant,
  registersAreEqual,
  addTwoRegisters
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

programCounterIncrement :: Word16
programCounterIncrement = 0x0002

--0x00E0
clearScreen :: Chip8 -> Chip8 
clearScreen chip8State = chip8State { graphics = V.replicate 2048 0x00 }

--0x00EE
returnFromSubroutine :: Chip8 -> Chip8
returnFromSubroutine chip8State =
  chip8State {
    stack = V.init originalStack, 
    stackPointer = originalStackPointer - 1,
    programCounter = lastAddress + programCounterIncrement
  }
  where 
    originalStack = stack chip8State
    originalStackPointer = stackPointer chip8State
    lastAddress = V.last originalStack

--0x1NNN
jumpToAddress :: Chip8 -> Chip8 
jumpToAddress chip8State = 
  chip8State { 
    programCounter = newAddress
  }
  where 
    opcode = currentOpcode chip8State
    newAddress = opcode .&. 0x0FFF

--0x2NNN
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

--0x3XNN
registerEqualsConstant :: Chip8 -> Chip8
registerEqualsConstant chip8State =
  chip8State {
    programCounter = if registerXValue == constant 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerX = (fromIntegral $ shiftR (opcode .&. 0x0F00) 8) :: Int
    registerXValue = originalVRegisters V.! registerX
    constant = (fromIntegral $ opcode .&. 0x00FF) :: Word8

--0x4XNN
registerDoesNotEqualConstant :: Chip8 -> Chip8
registerDoesNotEqualConstant chip8State =
  chip8State {
    programCounter = if registerXValue /= constant 
      then originalProgramCounter + (programCounterIncrement * 2)
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerX = (fromIntegral $ shiftR (opcode .&. 0x0F00) 8) :: Int
    registerXValue = originalVRegisters V.! registerX
    constant = (fromIntegral $ opcode .&. 0x00FF) :: Word8

--0x5XY0
registersAreEqual :: Chip8 -> Chip8
registersAreEqual chip8State =
  chip8State {   
    programCounter = if registerXValue == registerYValue 
      then originalProgramCounter + (programCounterIncrement * 2) 
      else originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    originalVRegisters = vRegisters chip8State 
    opcode = currentOpcode chip8State
    registerX = (fromIntegral $ shiftR (opcode .&. 0x0F00) 8) :: Int
    registerXValue = originalVRegisters V.! registerX
    registerY = (fromIntegral $ shiftR (opcode .&. 0x00F0) 4) :: Int
    registerYValue = originalVRegisters V.! registerY

--0x8XY4
addTwoRegisters :: Chip8 -> Chip8
addTwoRegisters chip8State = 
  chip8State {
    vRegisters = V.update originalVRegisters $ V.fromList [(registerX,total),(0xF,carry)],
    programCounter = originalProgramCounter + programCounterIncrement
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

    