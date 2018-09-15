module Emulator.Helpers
  ( hoist
  ) where

import Control.Monad.State

hoist :: Monad m => State s a -> StateT s m a
hoist = StateT . (return .) . runState