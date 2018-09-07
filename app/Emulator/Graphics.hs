module Emulator.Graphics
( drawGraphicsIfApplicable,
  drawGraphics
) where

import SDL
import Control.Monad.State
import Control.Lens
import Foreign.C.Types

import Chip8
import Types
import Emulator.Types
import Emulator.Helpers

drawGraphicsIfApplicable :: Renderer -> Texture -> Emulator Texture
drawGraphicsIfApplicable renderer texture = do
  canDrawGraphics <- gets (\givenState -> givenState^.drawFlag)
  if canDrawGraphics 
    then do
      updatedTexture <- drawGraphics renderer texture 
      modify (\givenState -> givenState & drawFlag .~ False)
      return updatedTexture
    else return texture

drawGraphics :: Renderer -> Texture -> Emulator Texture
drawGraphics renderer texture = do
  pixels <- hoist getGraphicsAsByteString
  updatedTexture <- liftIO $ updateTexture texture Nothing pixels (256 :: CInt)
  liftIO $ copy renderer updatedTexture Nothing Nothing
  liftIO $ present renderer
  return updatedTexture