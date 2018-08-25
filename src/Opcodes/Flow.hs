module Opcodes.Flow
( returnFromSubroutine,
  jumpToAddress,
  callSubroutine,
  jumpToAddressPlusRegisterZero
) where

import Data.Bits
import Data.Word
import qualified Data.Vector as V

import Types
import Constants
import Helpers

{-
  0x00EE
  Returns from a subroutine.
-}
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

{-
  0x1NNN
  Jumps to address NNN.
-}
jumpToAddress :: Chip8 -> Chip8 
jumpToAddress chip8State = 
  chip8State { 
    programCounter = newAddress
  }
  where 
    opcode = currentOpcode chip8State
    newAddress = parseThreeDigitConstant opcode 

{-
  0x2NNN
  Calls subroutine at NNN.
-}
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

{-
  0xBNNN
  Jumps to the address NNN plus V0.
-}
jumpToAddressPlusRegisterZero :: Chip8 -> Chip8 
jumpToAddressPlusRegisterZero chip8State =
  chip8State { 
    programCounter = newAddress
  }
  where 
    opcode = currentOpcode chip8State
    originalVRegisters = vRegisters chip8State 
    constant = parseThreeDigitConstant opcode
    registerZeroValue = originalVRegisters V.! 0x0
    convertedRegisterValue = (fromIntegral registerZeroValue) :: Word16
    newAddress = constant + convertedRegisterValue