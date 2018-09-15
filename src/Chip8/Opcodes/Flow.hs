module Chip8.Opcodes.Flow
  ( returnFromSubroutine
  , jumpToAddress
  , callSubroutine
  , jumpToAddressPlusRegisterZero
  ) where

import Data.Word
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens

import Chip8.Types
import Chip8.Constants
import Chip8.Helpers

{-
  0x00EE
  Returns from a subroutine.
-}
returnFromSubroutine :: Chip8 ()
returnFromSubroutine = do
  currentStackState <- gets (\chip8State -> chip8State^.stack)
  let lastAddress = V.last currentStackState
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
  currentProgramCounter <- gets (\chip8State -> chip8State^.programCounter)
  let storeAddressInStack = flip V.snoc currentProgramCounter 
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
  currentRegisterState <- gets (\chip8State -> chip8State^.vRegisters)
  constant <- parseThreeDigitConstant
  let registerZeroValue = currentRegisterState V.! 0x0
      convertedRegisterValue = fromIntegral registerZeroValue :: Word16
      newAddress = constant + convertedRegisterValue
  modify (\givenState -> givenState & programCounter .~ newAddress)