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
  consumer (\(nextIndex -> (i,s)) -> (d i,s))
           (\(nextIndex -> (i,s)) ->
              case M.lookup i (formletMap s) of
                Nothing -> (Failed (d i),s)
                Just a -> (Succeeded a,s))
  where d = Unit . Index
        nextIndex s =
          (formletIndex s,s {formletIndex = formletIndex s + 1})
