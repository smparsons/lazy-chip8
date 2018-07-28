module Cpu.Helpers
( parseRegisterXNumber,
  getRegisterXValue,
  parseRegisterYNumber,
  getRegisterYValue
) where

import Data.Word
import Data.Bits
import qualified Data.Vector as V

--Given an opcode with the format 0x*XY*, return X
parseRegisterXNumber :: Word16 -> Int 
parseRegisterXNumber opcode = (fromIntegral $ shiftR (opcode .&. 0x0F00) 8) :: Int

--Given an opcode with the format 0x*XY* and a vector of registers, return the value stored in register X
getRegisterXValue :: Word16 -> V.Vector Word8 -> Word8
getRegisterXValue opcode registers = registers V.! registerNumber where
  registerNumber = parseRegisterXNumber opcode

--Given an opcode with the format 0x*XY*, return Y
parseRegisterYNumber :: Word16 -> Int
parseRegisterYNumber opcode = (fromIntegral $ shiftR (opcode .&. 0x00F0) 4) :: Int

--Given an opcode with the format 0x*XY* and a vector of registers, return the value stored in register Y
getRegisterYValue :: Word16 -> V.Vector Word8 -> Word8
getRegisterYValue opcode registers = registers V.! registerNumber where 
  registerNumber = parseRegisterYNumber opcode