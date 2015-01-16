{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- | Validating indexed formlet with auto-generated input names.

module Descriptive.Formlet where

import           Descriptive

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

data Formlet
  = Index !Integer
  | Constrained !Text
  deriving (Show)

data FormletState =
  FormletState {formletMap :: (Map Integer Text)
               ,formletIndex :: !Integer}
  deriving (Show)

-- | Consume any character.
indexed :: Consumer FormletState Formlet Text
indexed =
  consumer (\(nextIndex -> (i,s)) -> (d i,s))
           (\(nextIndex -> (i,s)) ->
              case M.lookup i (formletMap s) of
                Nothing -> (Left (d i),s)
                Just a -> (Right a,s))
  where d = Unit . Index
        nextIndex s =
          (formletIndex s,s {formletIndex = formletIndex s + 1})
