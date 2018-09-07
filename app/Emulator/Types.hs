module Emulator.Types
( Emulator 
) where

import Control.Monad.State
import Types

type Emulator a = StateT Chip8State IO a