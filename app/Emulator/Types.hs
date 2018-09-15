module Emulator.Types
  ( Emulator
  ) where

import Chip8
import Control.Monad.State

type Emulator a = StateT Chip8State IO a
