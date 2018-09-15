module Emulator.Types
  ( Emulator
  ) where

import Control.Monad.State

import Chip8

type Emulator a = StateT Chip8State IO a
