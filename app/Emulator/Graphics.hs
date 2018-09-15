module Emulator.Graphics
  ( drawGraphicsIfNecessary
  ) where

import Chip8
import Control.Lens hiding (Context)
import Control.Monad.State
import Emulator.Helpers
import Emulator.Types
import Foreign.C.Types
import SDL

drawGraphicsIfNecessary :: Renderer -> Texture -> Emulator Texture
drawGraphicsIfNecessary renderer texture = do
  canDrawGraphics <- gets (\givenState -> givenState^.drawFlag)
  if canDrawGraphics 
    then do 
      pixels <- hoist getGraphicsAsByteString
      updatedTexture <- liftIO $ updateTexture texture Nothing pixels (256 :: CInt)
      liftIO $ copy renderer updatedTexture Nothing Nothing
      liftIO $ present renderer
      modify (\givenState -> givenState & drawFlag .~ False)
      return updatedTexture
    else return texture
