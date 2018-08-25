module Cpu
( executeOpcode,
  extractOpcodeFromMemory,
  decodeOpcode,
  parseDigitsFromOpcode,
  decrementDelayTimer,
  decrementSoundTimer
) where

import Data.Word
import Data.Bits
import qualified Data.Vector as V

import Types
import Opcodes.Assignment
import Opcodes.BitwiseOps
import Opcodes.Conditionals
import Opcodes.ConstantOps
import Opcodes.Display
import Opcodes.Flow
import Opcodes.KeyOps
import Opcodes.Math
import Opcodes.Memory
import Opcodes.Timer

executeOpcode :: Chip8 -> Chip8
executeOpcode chip8State = operation chip8State 
  where 
    originalProgramCounter = programCounter chip8State
    originalMemory = memory chip8State
    opcode = extractOpcodeFromMemory originalMemory originalProgramCounter
    operation = decodeOpcode opcode

extractOpcodeFromMemory :: V.Vector Word8 -> Word16 -> Word16
extractOpcodeFromMemory givenMemory givenProgramCounter = 
  leftShiftedOpcodeFirstHalf .|. opcodeSecondHalf
  where
    memoryIndex = fromIntegral givenProgramCounter :: Int
    opcodeFirstHalf = (fromIntegral $ givenMemory V.! memoryIndex) :: Word16
    opcodeSecondHalf = (fromIntegral $ givenMemory V.! (memoryIndex + 1)) :: Word16 
    leftShiftedOpcodeFirstHalf = opcodeFirstHalf `shiftL` 8

decodeOpcode :: Word16 -> (Chip8 -> Chip8)
decodeOpcode opcode =
  case parseDigitsFromOpcode opcode of
    (0x0, _, 0x0) -> clearScreen
    (0x0, _, 0xE) -> returnFromSubroutine
    (0x1, _, _) -> jumpToAddress
    (0x2, _, _) -> callSubroutine 
    (0x3, _, _) -> registerEqualsConstant
    (0x4, _, _) -> registerDoesNotEqualConstant
    (0x5, _, _) -> registersAreEqual
    (0x6, _, _) -> setRegisterToConstant
    (0x7, _, _) -> addConstantToRegister
    (0x8, _, 0x0) -> assignToRegister
    (0x8, _, 0x1) -> bitwiseOr
    (0x8, _, 0x2) -> bitwiseAnd 
    (0x8, _, 0x3) -> bitwiseXor
    (0x8, _, 0x4) -> addTwoRegisters
    (0x8, _, 0x5) -> subtractRegister
    (0x8, _, 0x6) -> shiftRight
    (0x8, _, 0x7) -> subtractTwoRegisters
    (0x8, _, 0xE) -> shiftLeft
    (0x9, _, _) -> registersAreNotEqual
    (0xA, _, _) -> setIndexRegisterToAddress
    (0xB, _, _) -> jumpToAddressPlusRegisterZero
    (0xC, _, _) -> randomBitwiseAnd
    (0xD, _, _) -> drawGraphics
    (0xE, _, 0xE) -> keyIsPressed
    (0xE, _, 0x1) -> keyIsNotPressed
    (0xF, _, 0x7) -> setRegisterToDelayTimer
    (0xF, _, 0xA) -> awaitKeyPress
    (0xF, 1, 0x5) -> setDelayTimerToRegister
    (0xF, _, 0x8) -> setSoundTimerToRegister
    (0xF, _, 0xE) -> addRegisterToIndexRegister
    (0xF, _, 0x9) -> storeSpriteLocation
    (0xF, _, 0x3) -> storeBCD
    (0xF, 0x5, 0x5) -> registerDump
    (0xF, 0x6, 0x5) -> registerLoad
    _ -> error "Invalid Opcode"

parseDigitsFromOpcode :: Word16 -> (Word16, Word16, Word16)
parseDigitsFromOpcode opcode = (firstDigit, thirdDigit, lastDigit)
  where 
    firstDigit = (opcode .&. 0xF000) `shiftR` 12
    thirdDigit = (opcode .&. 0x00F0) `shiftR` 4
    lastDigit = opcode .&. 0x000F

decrementDelayTimer :: Chip8 -> Chip8
decrementDelayTimer chip8State = 
  chip8State { 
    delayTimer = if originalDelayTimer > 0 then originalDelayTimer - 1 else originalDelayTimer 
  }
  where 
    originalDelayTimer = delayTimer chip8State

decrementSoundTimer :: Chip8 -> Chip8
decrementSoundTimer chip8State = 
  chip8State { 
    soundTimer = if originalSoundTimer > 0 then originalSoundTimer - 1 else originalSoundTimer 
  }
  where 
    originalSoundTimer = soundTimer chip8State