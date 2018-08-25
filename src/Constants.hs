module Constants
( programCounterIncrement,
  chip8NumberOfColumns
) where

import Data.Word

programCounterIncrement :: Word16
programCounterIncrement = 0x0002

chip8NumberOfColumns :: Int
chip8NumberOfColumns = 64