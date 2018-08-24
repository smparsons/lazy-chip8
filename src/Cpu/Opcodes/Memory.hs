module Cpu.Opcodes.Memory
( setIndexRegisterToAddress,
  addRegisterToIndexRegister,
  registerDump,
  registerLoad,
  storeBCD,
  storeSpriteLocation
) where

import qualified Data.Vector as V
import Data.Word

import Cpu.Helpers
import Cpu.Types
import Cpu.Constants

{-
  0xANNN
  Sets I to the address NNN.
-}
setIndexRegisterToAddress :: Chip8 -> Chip8 
setIndexRegisterToAddress chip8State =
  chip8State {
    indexRegister = address,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State 
    address = parseThreeDigitConstant opcode

{-
  0xFX1E
  Adds VX to I.
-}
addRegisterToIndexRegister :: Chip8 -> Chip8 
addRegisterToIndexRegister chip8State =
  chip8State {
    indexRegister = indexRegisterValue + convertedRegisterXValue,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State 
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    indexRegisterValue = indexRegister chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    convertedRegisterXValue = fromIntegral registerXValue :: Word16

{-
  0xFX55
  Stores V0 to VX (including VX) in memory starting at address I. The offset from I is increased 
  by 1 for each value written, but I itself is left unmodified.
-}
registerDump :: Chip8 -> Chip8 
registerDump chip8State =
  chip8State {
    memory = V.update originalMemory registerValuesToDump,
    programCounter = originalProgramCounter + programCounterIncrement 
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    originalMemory = memory chip8State
    opcode = currentOpcode chip8State 
    indexRegisterValue = indexRegister chip8State 
    registerXNumber = parseRegisterXNumber opcode
    numberOfRegisterValuesToSlice = registerXNumber + 1
    registersToProcess = V.slice 0 numberOfRegisterValuesToSlice originalVRegisters
    registerValuesToDump = 
      V.imap 
        (\index registerValue -> 
          let convertedAddress = (fromIntegral indexRegisterValue :: Int) in 
            (convertedAddress + index, registerValue))
        registersToProcess
{-
  0xFX65
  Fills V0 to VX (including VX) with values from memory starting at address I. The offset from I 
  is increased by 1 for each value written, but I itself is left unmodified.
-}
registerLoad :: Chip8 -> Chip8
registerLoad chip8State =
  chip8State {
    vRegisters = V.update originalVRegisters memoryValuesToLoad,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    originalMemory = memory chip8State
    opcode = currentOpcode chip8State
    indexRegisterValue = indexRegister chip8State
    registerXNumber = parseRegisterXNumber opcode
    numberOfMemoryValuesToSlice = registerXNumber + 1
    convertedIndexRegisterValue = fromIntegral indexRegisterValue :: Int
    memoryValuesToProcess = V.slice convertedIndexRegisterValue numberOfMemoryValuesToSlice originalMemory
    memoryValuesToLoad = V.imap (\index memoryValue -> (index, memoryValue)) memoryValuesToProcess

{-
  0xFX33
  Stores the binary-coded decimal representation of VX, with the most significant of three digits 
  at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2. 
  (In other words, take the decimal representation of VX, place the hundreds digit in memory at 
  location in I, the tens digit at location I+1, and the ones digit at location I+2.)
-}
storeBCD :: Chip8 -> Chip8
storeBCD chip8State =
  chip8State {
    memory = V.update originalMemory $ V.fromList
      [ (convertedIndexValue, registerXValue `div` 100)
      , (convertedIndexValue + 1, (registerXValue `div` 10) `mod` 10)
      , (convertedIndexValue + 2, (registerXValue `mod` 100) `mod` 10) ],
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    originalMemory = memory chip8State
    opcode = currentOpcode chip8State
    indexRegisterValue = indexRegister chip8State
    convertedIndexValue = fromIntegral indexRegisterValue :: Int
    registerXValue = getRegisterXValue opcode originalVRegisters

{-
  0xFX29
  Sets I to the location of the sprite for the character in VX. Characters 0-F  (in hexadecimal) 
  are represented by a 4x5 font.
-}
storeSpriteLocation :: Chip8 -> Chip8 
storeSpriteLocation chip8State =
  chip8State {
    indexRegister = spriteLocation,
    programCounter = originalProgramCounter + programCounterIncrement
  }
  where 
    originalVRegisters = vRegisters chip8State
    originalProgramCounter = programCounter chip8State
    opcode = currentOpcode chip8State
    registerXValue = getRegisterXValue opcode originalVRegisters
    spriteLocation = (fromIntegral $ registerXValue  * 0x5) :: Word16