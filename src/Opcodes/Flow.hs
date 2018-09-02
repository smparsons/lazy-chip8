module Opcodes.Flow
( returnFromSubroutine,
  jumpToAddress,
  callSubroutine,
  jumpToAddressPlusRegisterZero
) where

import Data.Word
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Types
import Constants
import Helpers

{-
  0x00EE
  Returns from a subroutine.
-}
returnFromSubroutine :: Chip8 ()
returnFromSubroutine = do
  chip8State <- get
  let lastAddress = V.last $ chip8State^.stack
  modify (\givenState -> givenState & stack %~ V.init)
  modify (\givenState -> givenState & stackPointer -~ 1)
  let newAddress = lastAddress + programCounterIncrement 
  modify (\givenState -> givenState & programCounter .~ newAddress)

{-
  0x1NNN
  Jumps to address NNN.
-}
jumpToAddress :: Chip8 ()
jumpToAddress = do
  newAddress <- parseThreeDigitConstant
  modify (\givenState -> givenState & programCounter .~ newAddress)

{-
  0x2NNN
  Calls subroutine at NNN.
-}
callSubroutine :: Chip8 ()
callSubroutine = do
  chip8State <- get
  let storeAddressInStack = flip V.snoc $ chip8State^.programCounter  
  modify (\givenState -> givenState & stack %~ storeAddressInStack)
  modify (\givenState -> givenState & stackPointer +~ 1)
  newAddress <- parseThreeDigitConstant
  modify (\givenState -> givenState & programCounter .~ newAddress)

{-
  0xBNNN
  Jumps to the address NNN plus V0.
-}
jumpToAddressPlusRegisterZero :: Chip8 () 
jumpToAddressPlusRegisterZero = do
  chip8State <- get
  constant <- parseThreeDigitConstant
  let registerZeroValue = (chip8State^.vRegisters) V.! 0x0
      convertedRegisterValue = fromIntegral registerZeroValue :: Word16
      newAddress = constant + convertedRegisterValue
  modify (\givenState -> givenState & programCounter .~ newAddress)