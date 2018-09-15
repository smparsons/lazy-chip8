module Opcodes.Memory
( setIndexRegisterToAddress,
  addRegisterToIndexRegister,
  registerDump,
  registerLoad,
  storeBCD,
  storeSpriteLocation
) where

import qualified Data.Vector as V
import Data.Word
import Control.Monad.State
import Control.Lens

import Helpers
import Types

{-
  0xANNN
  Sets I to the address NNN.
-}
setIndexRegisterToAddress :: Chip8 ()
setIndexRegisterToAddress = do
  address <- parseThreeDigitConstant
  modify (\givenState -> givenState & indexRegister .~ address)
  incrementProgramCounter

{-
  0xFX1E
  Adds VX to I.
-}
addRegisterToIndexRegister :: Chip8 () 
addRegisterToIndexRegister = do
  registerXValue <- getRegisterXValue
  let convertedRegisterXValue = fromIntegral registerXValue :: Word16
  modify (\givenState -> givenState & indexRegister +~ convertedRegisterXValue)
  incrementProgramCounter

{-
  0xFX55
  Stores V0 to VX (including VX) in memory starting at address I. The offset from I is increased 
  by 1 for each value written, but I itself is left unmodified.
-}
registerDump :: Chip8 ()
registerDump = do
  currentRegisterState <- gets (\chip8State -> chip8State^.vRegisters)
  currentIndexRegister <- fmap fromIntegral $ gets (\chip8State -> chip8State^.indexRegister)
  registerXNumber <- parseRegisterXNumber
  let registersToProcess = V.slice 0 (registerXNumber + 1) currentRegisterState
      mapMemoryAddressAndValue = (\currentIndex registerValue -> (currentIndexRegister + currentIndex, registerValue))
      registerValuesToDump = V.imap mapMemoryAddressAndValue registersToProcess
      dumpRegisters = flip V.update registerValuesToDump
  modify (\givenState -> givenState & memory %~ dumpRegisters)
  incrementProgramCounter

{-
  0xFX65
  Fills V0 to VX (including VX) with values from memory starting at address I. The offset from I 
  is increased by 1 for each value written, but I itself is left unmodified.
-}
registerLoad :: Chip8 ()
registerLoad = do
  currentMemoryState <- gets (\chip8State -> chip8State^.memory)
  currentIndexRegister <- fmap fromIntegral $ gets (\chip8State -> chip8State^.indexRegister)
  registerXNumber <- parseRegisterXNumber
  let memoryValuesToProcess = V.slice currentIndexRegister (registerXNumber + 1) currentMemoryState
      mapRegisterAndValue = (\currentIndex memoryValue -> (currentIndex, memoryValue))
      memoryValuesToLoad = V.imap mapRegisterAndValue memoryValuesToProcess
      loadMemory = flip V.update memoryValuesToLoad
  modify (\givenState -> givenState & vRegisters %~ loadMemory)
  incrementProgramCounter

{-
  0xFX33
  Stores the binary-coded decimal representation of VX, with the most significant of three digits 
  at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2. 
  (In other words, take the decimal representation of VX, place the hundreds digit in memory at 
  location in I, the tens digit at location I+1, and the ones digit at location I+2.)
-}
storeBCD :: Chip8 ()
storeBCD = do
  currentIndexRegister <- fmap fromIntegral $ gets (\chip8State -> chip8State^.indexRegister)
  registerXValue <- getRegisterXValue
  let memoryModifications = V.fromList
        [ (currentIndexRegister, registerXValue `div` 100)
        , (currentIndexRegister + 1, (registerXValue `div` 10) `mod` 10)
        , (currentIndexRegister + 2, (registerXValue `mod` 100) `mod` 10) ]
      updateMemory = flip V.update memoryModifications
  modify (\givenState -> givenState & memory %~ updateMemory)
  incrementProgramCounter 

{-
  0xFX29
  Sets I to the location of the sprite for the character in VX. Characters 0-F  (in hexadecimal) 
  are represented by a 4x5 font.
-}
storeSpriteLocation :: Chip8 () 
storeSpriteLocation = do
  registerXValue <- getRegisterXValue
  let spriteLocation = (fromIntegral $ registerXValue * 0x5) :: Word16
  modify (\givenState -> givenState & indexRegister .~ spriteLocation)
  incrementProgramCounter