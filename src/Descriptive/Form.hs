{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Validating form with named inputs.

module Descriptive.Form
  (-- * Combinators
   input
  -- * Description
  ,Form (..)
  )
  where

import           Descriptive

import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

-- | Form descriptor.
data Form
  = Input !Text
  | Constraint !Text
  deriving (Show,Eq)

-- | Consume any input value.
input :: Text -> Consumer (Map Text Text) Form Text
input name =
  consumer (return d)
           (do s <- get
               return (case M.lookup name s of
                         Nothing -> Continued d
                         Just a -> Succeeded a))
  where d = Unit (Input name)
