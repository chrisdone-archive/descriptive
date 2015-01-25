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
  (-- * Combinators
   object
  ,key
  ,keyMaybe
  ,array
  ,string
  ,integer
  ,double
  ,bool
  ,null
  ,label
  -- * Description
  ,Doc(..)
  )
  where

import           Data.Scientific
import           Descriptive

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
data Doc
  = Integer !Text
  | Double !Text
  | Text !Text
  | Boolean !Text
  | Null !Text
  | Object !Text
  | Key !Text
  | Array !Text
  | Label !Text
  deriving (Eq,Show,Typeable,Data)

-- | Consume an object.
object :: Text -- ^ Description of what the object is.
       -> Consumer Object Doc a -- ^ An object consumer.
       -> Consumer Value Doc a
object desc =
  wrap (\v d -> (Wrap doc (fst (d mempty)),v))
       (\v _ p ->
          case fromJSON v of
            Error{} -> (Failed (Unit doc),v)
            Success o ->
              (case p o of
                 (Failed e,_) -> Failed (Wrap doc e)
                 (Continued e,_) -> Failed (Wrap doc e)
                 (Succeeded a,_) -> Succeeded a
              ,toJSON o))
  where doc = Object desc

-- | Consume from object at the given key.
key :: Text -- ^ The key to lookup.
    -> Consumer Value Doc a -- ^ A value consumer of the object at the key.
    -> Consumer Object Doc a
key k =
  wrap (\o d ->
          first (Wrap doc)
                (second (const o)
                        (d (toJSON o))))
       (\o _ p ->
          case parseMaybe (const (o .: k))
                          () of
            Nothing -> (Failed (Unit doc),o)
            Just (v :: Value) ->
              first (bimap (Wrap doc) id)
                    (second (const o)
                            (p v)))
  where doc = Key k

-- | Optionally consume from object at the given key, only if it
-- exists.
keyMaybe :: Text -- ^ The key to lookup.
         -> Consumer Value Doc a -- ^ A value consumer of the object at the key.
         -> Consumer Object Doc (Maybe a)
keyMaybe k =
  wrap (\o d ->
          first (Wrap doc)
                (second (const o)
                        (d (toJSON o))))
       (\o _ p ->
          case parseMaybe (const (o .: k))
                          () of
            Nothing -> (Succeeded Nothing,o)
            Just (v :: Value) ->
              first (bimap (Wrap doc) Just)
                    (second (const o)
                            (p v)))
  where doc = Key k

-- | Consume an array.
array :: Text -- ^ Description of this array.
      -> Consumer Value Doc a -- ^ Consumer for each element in the array.
      -> Consumer Value Doc (Vector a)
array desc =
  wrap (\v d -> (Wrap doc (fst (d Aeson.Null)),v))
       (\v _ p ->
          case fromJSON v of
            Error{} -> (Failed (Unit doc),v)
            Success (o :: Vector Value) ->
              (fix (\loop i acc ->
                      if i < V.length o - 1
                         then case p (o ! i) of
                                (Failed e,_) ->
                                  Failed (Wrap doc e)
                                (Continued e,_) ->
                                  Failed (Wrap doc e)
                                (Succeeded a,_) ->
                                  loop (i + 1) (a : acc)
                         else Succeeded (V.fromList (reverse acc)))
                   0
                   []
              ,v))
  where doc = Array desc

-- | Consume a string.
string :: Text -- ^ Description of what the string is for.
       -> Consumer Value Doc Text
string doc =
  consumer (d,)
           (\s ->
              case fromJSON s of
                Error{} -> (Failed d,s)
                Success a -> (Succeeded a,s))
  where d = Unit (Text doc)

-- | Consume an integer.
integer :: Text -- ^ Description of what the integer is for.
        -> Consumer Value Doc Integer
integer doc =
  consumer (d,)
           (\s ->
              case s of
                Number a
                  | Right i <- floatingOrInteger a ->
                    (Succeeded i,s)
                _ -> (Failed d,s))
  where d = Unit (Integer doc)

-- | Consume an double.
double :: Text -- ^ Description of what the double is for.
        -> Consumer Value Doc Double
double doc =
  consumer (d,)
           (\s ->
              case s of
                Number a ->
                  (Succeeded (toRealFloat a),s)
                _ -> (Failed d,s))
  where d = Unit (Double doc)

-- | Parse a boolean.
bool :: Text -- ^ Description of what the bool is for.
     -> Consumer Value Doc Bool
bool doc =
  consumer (d,)
           (\s ->
              case fromJSON s of
                Error{} -> (Failed d,s)
                Success a -> (Succeeded a,s))
  where d = Unit (Boolean doc)

-- | Expect null.
null :: Text -- ^ What the null is for.
       -> Consumer Value Doc ()
null doc =
  consumer (d,)
           (\s ->
              case fromJSON s of
                Success Aeson.Null -> (Succeeded (),s)
                _ -> (Failed d,s))
  where d = Unit (Null doc)

-- | Wrap a consumer with a label containing additional description.
label :: Text -- ^ Some label.
      -> Consumer s Doc a -- ^ An object consumer.
      -> Consumer s Doc a
label desc =
  wrap (\s d -> (Wrap doc (fst (d s)),s))
       (\s _ p -> p s)
  where doc = Label desc
