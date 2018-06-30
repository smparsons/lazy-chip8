module Main where

import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

rgbaForWhite :: [Word8]
rgbaForWhite = [255, 255, 255, 255]

totalBytesForEmulatorGraphics :: Int
totalBytesForEmulatorGraphics = 8192

bitmapData :: ByteString 
bitmapData = pack $ take totalBytesForEmulatorGraphics (cycle rgbaForWhite)

originalPicture :: Picture
originalPicture = bitmapOfByteString 64 32 (BitmapFormat TopToBottom PxRGBA) bitmapData True

scaledPicture :: Picture
scaledPicture = Scale 5 5 originalPicture

main :: IO ()
main = do
  display (InWindow "Lazy-Chip8" (640, 320) (10, 10)) black scaledPicture
