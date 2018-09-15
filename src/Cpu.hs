module Cpu
( executeOpcode,
  extractOpcodeFromMemory,
  parseDigitsFromOpcode,
  decrementDelayTimer,
  decrementSoundTimer
) where

import Data.Word
import Data.Bits
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

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

executeOpcode :: Chip8 ()
executeOpcode = do
  extractOpcodeFromMemory
  opcodeDigits <- parseDigitsFromOpcode
  case opcodeDigits of
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

extractOpcodeFromMemory :: Chip8 ()
extractOpcodeFromMemory = do
  currentMemoryState <- gets (\chip8State -> chip8State^.memory)
  memoryIndex <- fmap fromIntegral $ gets (\chip8State -> chip8State^.programCounter)
  let opcodeFirstHalf = (fromIntegral $ currentMemoryState V.! memoryIndex) :: Word16
      opcodeSecondHalf = (fromIntegral $ currentMemoryState V.! (memoryIndex + 1)) :: Word16
      leftShiftedOpcodeFirstHalf = opcodeFirstHalf `shiftL` 8
  modify (\givenState -> givenState & currentOpcode .~ (leftShiftedOpcodeFirstHalf .|. opcodeSecondHalf))

parseDigitsFromOpcode :: Chip8 (Word16, Word16, Word16)
parseDigitsFromOpcode = do
  opcode <- gets (\chip8State -> chip8State^.currentOpcode)
  let firstDigit = (opcode .&. 0xF000) `shiftR` 12
      thirdDigit = (opcode .&. 0x00F0) `shiftR` 4
      lastDigit = opcode .&. 0x000F
  return (firstDigit, thirdDigit, lastDigit)

decrementDelayTimer :: Chip8 ()
decrementDelayTimer = do 
  originalDelayTimer <- gets (\chip8State -> chip8State^.delayTimer)
  let newDelayTimer = if originalDelayTimer > 0 then originalDelayTimer - 1 else originalDelayTimer
  modify (\givenState -> givenState & delayTimer .~ newDelayTimer)    

decrementSoundTimer :: Chip8 ()
decrementSoundTimer = do
  originalSoundTimer <- gets (\chip8State -> chip8State^.soundTimer)
  let shouldPlaySound = originalSoundTimer == 1
      newSoundTimer = if originalSoundTimer > 0 then originalSoundTimer - 1 else originalSoundTimer
  modify (\givenState -> givenState & audioFlag .~ shouldPlaySound)
  modify (\givenState -> givenState & soundTimer .~ newSoundTimer)