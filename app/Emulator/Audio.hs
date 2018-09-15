module Emulator.Audio 
  ( createBeepSource
  , playBeepIfNecessary
  ) where

import Control.Lens
import Control.Monad.State
import Sound.ALUT

import Emulator.Types

import Chip8

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