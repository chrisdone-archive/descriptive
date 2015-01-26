{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- | Validating indexed formlet with auto-generated input names.

module Descriptive.Formlet
  (-- * Combinators
   indexed
  ,FormletState(..)
  -- * Description
  ,Formlet(..))
  where

import           Descriptive

import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

-- | Description of a formlet.
data Formlet
  = Index !Integer
  | Constrained !Text
  deriving (Show,Eq)

-- | State used when running a formlet.
data FormletState =
  FormletState {formletMap :: (Map Integer Text)
               ,formletIndex :: !Integer}
  deriving (Show,Eq)

-- | Consume any character.
indexed :: Consumer FormletState Formlet Text
indexed =
  consumer (do i <- nextIndex
               return (d i))
           (do i <- nextIndex
               s <- get
               return (case M.lookup i (formletMap s) of
                         Nothing -> Failed (d i)
                         Just a -> Succeeded a))
  where d = Unit . Index
        nextIndex :: MonadState FormletState m => m Integer
        nextIndex =
          do i <- gets formletIndex
             modify (\s ->
                       s {formletIndex = formletIndex s + 1})
             return i
