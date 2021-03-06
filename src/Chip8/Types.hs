{-# LANGUAGE TemplateHaskell #-}

module Chip8.Types
  ( Chip8
  , KeyPressState(..)
  , Chip8State(..)
  , currentOpcode
  , memory
  , vRegisters
  , indexRegister
  , programCounter
  , graphics
  , delayTimer
  , soundTimer
  , stack
  , stackPointer
  , keyState
  , drawFlag
  , audioFlag
  , randomNumberSeed
  ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V
import Data.Word
import System.Random

data KeyPressState 
  = Pressed 
  | Released
  deriving (Eq, Show)

data Chip8State = Chip8State {
  _currentOpcode :: Word16,
  _memory :: V.Vector Word8,
  _vRegisters :: V.Vector Word8,
  _indexRegister :: Word16,
  _programCounter :: Word16,
  _graphics :: V.Vector Word8,
  _delayTimer :: Word8,
  _soundTimer :: Word8,
  _stack :: V.Vector Word16,
  _stackPointer :: Word16,
  _keyState :: V.Vector KeyPressState,
  _drawFlag :: Bool,
  _audioFlag :: Bool,
  _randomNumberSeed :: StdGen
} deriving (Show)

makeLenses ''Chip8State

type Chip8 = State Chip8State