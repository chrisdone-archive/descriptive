{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Validating form with named inputs.

module Descriptive.Form
  (-- * Combinators
   input
  ,validate
  -- * Description
  ,Form (..)
  )
  where

import           Descriptive

import           Control.Arrow
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

-- | Form descriptor.
data Form
  = Input !Text
  | Constraint !Text
  deriving (Show)

-- | Consume any input value.
input :: Text -> Consumer (Map Text Text) Form Text
input name =
  consumer (d,)
           (\s ->
              (case M.lookup name s of
                 Nothing -> Left d
                 Just a -> Right a
              ,s))
  where d = Unit (Input name)

-- | Validate a form input with a description of what's required.
validate :: Text -- ^ Description of what it expects.
         -> (a -> Maybe b) -- ^ Attempt to parse the value.
         -> Consumer (Map Text Text) Form a -- ^ Consumer to add validation to.
         -> Consumer (Map Text Text) Form b
validate d' check =
  wrap (\s d -> redescribe (d s))
       (\s d p ->
          case p s of
            (Left e,s') -> (Left e,s')
            (Right a,s') ->
              case check a of
                Nothing ->
                  (Left (fst (redescribe (d s))),s')
                Just a' -> (Right a',s'))
  where redescribe = first (Wrap (Constraint d'))
