{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A JSON API which describes itself.

module Descriptive.JSON
  (-- * Consumers
   object
  ,key
  ,keyMaybe
  ,array
  ,string
  ,integer
  ,double
  ,bool
  ,null
  -- * Annotations
  ,label
  -- * Description
  ,Doc(..)
  )
  where

import           Descriptive
import           Descriptive.Internal

import           Control.Monad.State.Strict
import           Data.Scientific
import           Data.Function
import           Data.Aeson hiding (Value(Object,Null,Array),object)
import           Data.Aeson.Types (Value,parseMaybe)
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor
import           Data.Data
import           Data.Monoid
import           Data.Text (Text)
import           Data.Vector ((!))
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Prelude hiding (null)

-- | Description of parseable things.
data Doc a
  = Integer !Text
  | Double !Text
  | Text !Text
  | Boolean !Text
  | Null !Text
  | Object !Text
  | Key !Text
  | Array !Text
  | Label !a
  deriving (Eq,Show,Typeable,Data)

-- | Consume an object.
object :: Monad m
       => Text -- ^ Description of what the object is.
       -> Consumer Object (Doc d) m a -- ^ An object consumer.
       -> Consumer Value (Doc d) m a
object desc =
  wrap (\d ->
          do s <- get
             runSubStateT (const mempty)
                          (const s)
                          (liftM (Wrap doc) d))
       (\_ p ->
          do v <- get
             case fromJSON v of
               Error{} ->
                 return (Continued (Unit doc))
               Success (o :: Object) ->
                 do s <- get
                    runSubStateT
                      (const o)
                      (const s)
                      (do r <- p
                          case r of
                            Failed e ->
                              return (Continued (Wrap doc e))
                            Continued e ->
                              return (Continued (Wrap doc e))
                            Succeeded a ->
                              return (Succeeded a)))
  where doc = Object desc

-- | Consume from object at the given key.
key :: Monad m
    => Text -- ^ The key to lookup.
    -> Consumer Value (Doc d) m a -- ^ A value consumer of the object at the key.
    -> Consumer Object (Doc d) m a
key k =
  wrap (\d ->
          do s <- get
             runSubStateT toJSON
                          (const s)
                          (liftM (Wrap doc) d))
       (\_ p ->
          do s <- get
             case parseMaybe (const (s .: k))
                             () of
               Nothing ->
                 return (Continued (Unit doc))
               Just (v :: Value) ->
                 do r <-
                      runSubStateT (const v)
                                   (const s)
                                   p
                    return (bimap (Wrap doc) id r))
  where doc = Key k

-- | Optionally consume from object at the given key, only if it
-- exists.
keyMaybe :: Monad m
         => Text -- ^ The key to lookup.
         -> Consumer Value (Doc d) m a -- ^ A value consumer of the object at the key.
         -> Consumer Object (Doc d) m (Maybe a)
keyMaybe k =
  wrap (\d ->
          do s <- get
             runSubStateT toJSON
                          (const s)
                          (liftM (Wrap doc) d))
       (\_ p ->
          do s <- get
             case parseMaybe (const (s .: k))
                             () of
               Nothing ->
                 return (Succeeded Nothing)
               Just (v :: Value) ->
                 do r <-
                      runSubStateT (const v)
                                   (const s)
                                   p
                    return (bimap (Wrap doc) Just r))
  where doc = Key k

-- | Consume an array.
array :: Monad m
      => Text -- ^ Description of this array.
      -> Consumer Value (Doc d) m a -- ^ Consumer for each element in the array.
      -> Consumer Value (Doc d) m (Vector a)
array desc =
  wrap (\d -> liftM (Wrap doc) d)
       (\_ p ->
          do s <- get
             case fromJSON s of
               Error{} ->
                 return (Continued (Unit doc))
               Success (o :: Vector Value) ->
                 fix (\loop i acc ->
                        if i < V.length o - 1
                           then do r <-
                                     runSubStateT (const (o ! i))
                                                  (const s)
                                                  p
                                   case r of
                                     Failed e ->
                                       return (Continued (Wrap doc e))
                                     Continued e ->
                                       return (Continued (Wrap doc e))
                                     Succeeded a ->
                                       loop (i + 1)
                                            (a : acc)
                           else return (Succeeded (V.fromList (reverse acc))))
                     0
                     [])
  where doc = Array desc

-- | Consume a string.
string :: Monad m
       => Text -- ^ Description of what the string is for.
       -> Consumer Value (Doc d) m Text
string doc =
  consumer (return d)
           (do s <- get
               case fromJSON s of
                 Error{} -> return (Continued d)
                 Success a ->
                   return (Succeeded a))
  where d = Unit (Text doc)

-- | Consume an integer.
integer :: Monad m
        => Text -- ^ Description of what the integer is for.
        -> Consumer Value (Doc d) m Integer
integer doc =
  consumer (return d)
           (do s <- get
               case s of
                 Number a
                   | Right i <- floatingOrInteger a ->
                     return (Succeeded i)
                 _ -> return (Continued d))
  where d = Unit (Integer doc)

-- | Consume an double.
double :: Monad m
       => Text -- ^ Description of what the double is for.
       -> Consumer Value (Doc d) m Double
double doc =
  consumer (return d)
           (do s <- get
               case s of
                 Number a ->
                   return (Succeeded (toRealFloat a))
                 _ -> return (Continued d))
  where d = Unit (Double doc)

-- | Parse a boolean.
bool :: Monad m
     => Text -- ^ Description of what the bool is for.
     -> Consumer Value (Doc d) m Bool
bool doc =
  consumer (return d)
           (do s <- get
               case fromJSON s of
                 Error{} -> return (Continued d)
                 Success a ->
                   return (Succeeded a))
  where d = Unit (Boolean doc)

-- | Expect null.
null :: Monad m
     => Text -- ^ What the null is for.
     -> Consumer Value (Doc d) m ()
null doc =
  consumer (return d)
           (do s <- get
               case fromJSON s of
                 Success Aeson.Null ->
                   return (Succeeded ())
                 _ -> return (Continued d))
  where d = Unit (Null doc)

-- | Wrap a consumer with a label e.g. a type tag.
label :: Monad m
      => d                      -- ^ Some label.
      -> Consumer s (Doc d) m a -- ^ A value consumer.
      -> Consumer s (Doc d) m a
label desc =
  wrap (liftM (Wrap doc))
       (\_ p ->
          do r <- p
             case r of
               Failed e ->
                 return (Continued (Wrap doc e))
               k -> return k)
  where doc = Label desc
