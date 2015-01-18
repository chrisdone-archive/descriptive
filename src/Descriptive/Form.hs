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
  deriving (Show,Eq)

-- | Consume any input value.
input :: Text -> Consumer (Map Text Text) Form Text
input name =
  consumer (d,)
           (\s ->
              (case M.lookup name s of
                 Nothing -> Continued d
                 Just a -> Succeeded a
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
            (Failed e,s') -> (Failed e,s')
            (Continued e,s') -> (Continued (wrapper e),s')
            (Succeeded a,s') ->
              case check a of
                Nothing ->
                  (Continued (fst (redescribe (d s))),s')
                Just a' -> (Succeeded a',s'))
  where redescribe = first wrapper
        wrapper = Wrap (Constraint d')
