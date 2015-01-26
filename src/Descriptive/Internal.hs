-- | Internal functions not necessary to be exported.

module Descriptive.Internal where

import Control.Monad.State.Strict

-- | Run a different state in this state monad.
runSubStateT :: Monad m
             => (s -> s') -> (s' -> s) -> StateT s' m a -> StateT s m a
runSubStateT to from m =
  StateT (\s ->
            liftM (\(a,s') -> (a,from s'))
                  (runStateT m (to s)))
