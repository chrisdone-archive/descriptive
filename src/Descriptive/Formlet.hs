{-# LANGUAGE FlexibleContexts #-}

-- | Validating indexed formlet with auto-generated input names.

-- Examples:
--
-- λ> describeFormlet ((,) <$> indexed <*> indexed)
-- And (Unit (Index 0)) (Unit (Index 1))
-- λ> parseFormlet (M.fromList [(0,"chrisdone"),(1,"god")]) ((,) <$> indexed <*> indexed)
-- Right ("chrisdone","god")
-- λ> parseFormlet (M.fromList [(0,"chrisdone")]) ((,) <$> indexed <*> indexed)
-- Left (Unit (Index 2))

module Descriptive.Formlet where

import           Descriptive

import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)

data Formlet
  = Index !Integer
  | Constrained !Text
  deriving (Show)

data FormletState =
  FormletState {formletMap :: (Map Integer Text)
               ,formletIndex :: !Integer}

-- | Consume any character.
indexed :: MonadState FormletState m
        => Consumer Formlet m Text
indexed =
  consume (do i <- nextIndex
              return (Unit (Index i)))
          (\m ->
             do i <- nextIndex
                return (case M.lookup i (formletMap m) of
                          Nothing -> Nothing
                          Just value ->
                            Just value))
  where nextIndex =
          do i <- gets formletIndex
             modify (\fs ->
                       fs {formletIndex = i + 1})
             return i

-- | Parse a list of something.
parseFormlet :: Map Integer Text -> Consumer d (State (FormletState)) a -> Either (Description d) a
parseFormlet st m = evalState (consumer m) (FormletState st 0)

-- | Describe a consumer.
describeFormlet :: Consumer d (State (FormletState)) a -> Description d
describeFormlet (Consumer desc _) = evalState desc (FormletState mempty 0)
