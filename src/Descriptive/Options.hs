{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command-line options parser.

module Descriptive.Options where

import Descriptive
import Data.List
import Data.Monoid
import Data.Text (Text)

data Option
  = AnyString !Text
  | Constant !Text
  | Flag !Text !Text
  | Arg !Text !Text
  deriving (Show)

-- | Consume one argument from the argument list.
anyString :: Text -> Consumer [Text] Option Text
anyString help =
  consumer (d,)
           (\s ->
              case s of
                [] -> (Left d,s)
                (x:s') -> (Right x,s'))
  where d = Unit (AnyString help)

-- | Consume one argument from the argument list.
constant :: Text -> Consumer [Text] Option Text
constant x' =
  consumer (d,)
           (\s ->
              case s of
                (x:s') | x == x' ->
                  (Right x,s')
                _ -> (Left d,s))
  where d = Unit (Constant x')

-- | Consume a short boolean flag.
flag :: Text -> Text -> Consumer [Text] Option Bool
flag name help =
  consumer (d,)
           (\s ->
              (Right (elem ("-f" <> name) s),s))
  where d = Unit (Flag name help)

-- | Consume a named argument.
arg :: Text -> Text -> Consumer [Text] Option Text
arg name help =
  consumer (d,)
           (\s ->
              let indexedArgs =
                    zip [0 :: Integer ..] s
              in case find ((== "--" <> name) . snd) indexedArgs of
                   Nothing -> (Left d,s)
                   Just (i,_) ->
                     case lookup (i + 1) indexedArgs of
                       Nothing -> (Left d,s)
                       Just text ->
                         (Right text,s))
  where d = Unit (Arg name help)
