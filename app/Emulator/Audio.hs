module Emulator.Audio 
  ( createBeepSource
  , playBeepIfNecessary
  ) where

import Chip8
import Control.Lens
import Control.Monad.State
import Emulator.Types
import Sound.ALUT

createBeepSource :: IO Source
createBeepSource = do
  beep <- createBuffer $ Sine 340 0 0.075
  [source] <- genObjectNames 1
  queueBuffers source [beep]
  return source

playBeepIfNecessary :: Source -> Emulator ()
playBeepIfNecessary beep = do
  shouldPlayBeep <- gets (\givenState -> givenState^.audioFlag)
  if shouldPlayBeep 
    then do
      liftIO $ play [beep]
      modify (\givenState -> givenState & audioFlag .~ False)
    else return ()