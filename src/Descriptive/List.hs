-- | Consuming from lists.

module Descriptive.List where

import Descriptive

import Control.Monad.State.Strict

-- | Parse a list of something.
parseList :: [s] -> Consumer d (State [s]) a -> Either (Description d) a
parseList st m = evalState (consumer m) st

-- | Describe a consumer.
describeList :: Consumer d (State [s]) a -> Description d
describeList (Consumer desc _) = evalState desc []
