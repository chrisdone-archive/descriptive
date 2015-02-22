{-# LANGUAGE RankNTypes #-}
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

import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

-- | Form descriptor.
data Form d
  = Input !Text
  | Constraint !d
  deriving (Show,Eq)

-- | Consume any input value.
input :: Monad m => Text -> Consumer (Map Text Text) (Form d) m Text
input name =
  consumer (return d)
           (do s <- get
               return (case M.lookup name s of
                         Nothing -> Continued d
                         Just a -> Succeeded a))
  where d = Unit (Input name)

-- | Validate a form input with a description of what's required.
validate :: Monad m
         => d                           -- ^ Description of what it expects.
         -> (a -> StateT s m (Maybe b)) -- ^ Attempt to parse the value.
         -> Consumer s (Form d) m a     -- ^ Consumer to add validation to.
         -> Consumer s (Form d) m b     -- ^ A new validating consumer.
validate d' check =
  wrap (liftM wrapper)
       (\d p ->
          do s <- get
             r <- p
             case r of
               (Failed e) -> return (Failed e)
               (Continued e) ->
                 return (Continued (wrapper e))
               (Succeeded a) ->
                 do r' <- check a
                    case r' of
                      Nothing ->
                        do doc <- withStateT (const s) d
                           return (Continued (wrapper doc))
                      Just a' -> return (Succeeded a'))
  where wrapper = Wrap (Constraint d')
