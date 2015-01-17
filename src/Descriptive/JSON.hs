{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A JSON API which describes itself.

module Descriptive.JSON where

import Data.Bifunctor
import Data.Monoid
import Descriptive

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)

-- | Description of parseable things.
data Doc
  = Integer !Text
  | Text !Text
  | Struct !Text
  | Key !Text
  deriving (Show)

-- | Consume an object.
obj :: Text -> Consumer Object Doc a -> Consumer Value Doc a
obj desc =
  wrap (\v d -> (Wrap doc (fst (d mempty)),v))
       (\v _ p ->
          case fromJSON v of
            Error{} -> (Left (Unit doc),v)
            Success o ->
              (case p o of
                 (Left e,_) -> Left (Wrap doc e)
                 (Right a,_) -> Right a
              ,toJSON o))
  where doc = Struct desc

-- | Consume from object at the given key.
key :: Text -> Consumer Value Doc a -> Consumer Object Doc a
key k =
  wrap (\o d ->
          first (Wrap doc)
                (second (const o)
                        (d (toJSON o))))
       (\o _ p ->
          case parseMaybe (const (o .: k))
                          () of
            Nothing -> (Left (Unit doc),o)
            Just (v :: Value) ->
              first (bimap (Wrap doc) id)
                    (second (const o)
                            (p v)))
  where doc = Key k

-- | Consume a string.
string :: Text -> Consumer Value Doc Text
string doc =
  consumer (d,)
           (\s ->
              case fromJSON s of
                Error{} -> (Left d,s)
                Success a -> (Right a,s))
  where d = Unit (Text doc)

-- | Consume an integer.
integer :: Text -> Consumer Value Doc Integer
integer doc =
  consumer (d,)
           (\s ->
              case fromJSON s of
                Error{} -> (Left d,s)
                Success a -> (Right a,s))
  where d = Unit (Integer doc)
