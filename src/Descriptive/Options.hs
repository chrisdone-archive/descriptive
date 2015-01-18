{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command-line options parser.

module Descriptive.Options
  (-- * Combinators
   anyString
  ,constant
  ,flag
  ,prefix
  ,arg
  -- * Description
  ,Option(..)
  ,textDescription
  ,textOpt)
  where

import           Descriptive

import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Description of a commandline option.
data Option
  = AnyString !Text
  | Constant !Text
  | Flag !Text !Text
  | Arg !Text !Text
  | Prefix !Text !Text
  deriving (Show,Eq)

-- | Consume one argument from the argument list.
anyString :: Text -> Consumer [Text] Option Text
anyString help =
  consumer (d,)
           (\s ->
              case s of
                [] -> (Failed d,s)
                (x:s') -> (Succeeded x,s'))
  where d = Unit (AnyString help)

-- | Consume one argument from the argument list.
constant :: Text -> Consumer [Text] Option Text
constant x' =
  consumer (d,)
           (\s ->
              case s of
                (x:s') | x == x' ->
                  (Succeeded x,s')
                _ -> (Failed d,s))
  where d = Unit (Constant x')

-- | Find a short boolean flag.
flag :: Text -> Text -> Consumer [Text] Option Bool
flag name help =
  consumer (d,)
           (\s ->
              (Succeeded (elem ("--" <> name) s),filter (/= "--" <> name) s))
  where d = Unit (Flag name help)

-- | Find an argument prefixed by -X.
prefix :: Text -> Text -> Consumer [Text] Option Text
prefix pref help =
  consumer (d,)
           (\s ->
              case find (T.isPrefixOf ("-" <> pref)) s of
                Nothing -> (Failed d,s)
                Just a -> (Succeeded (T.drop (T.length pref + 1) a), delete a s))
  where d = Unit (Prefix pref help)

-- | Find a named argument.
arg :: Text -> Text -> Consumer [Text] Option Text
arg name help =
  consumer (d,)
           (\s ->
              let indexedArgs =
                    zip [0 :: Integer ..] s
              in case find ((== "--" <> name) . snd) indexedArgs of
                   Nothing -> (Failed d,s)
                   Just (i,_) ->
                     case lookup (i + 1) indexedArgs of
                       Nothing -> (Failed d,s)
                       Just text ->
                         (Succeeded text
                         ,map snd (filter (\(j,_) -> j /= i && j /= i + 1) indexedArgs)))
  where d = Unit (Arg name help)

-- | Make a text description of the command line options.
textDescription :: Description Option -> Text
textDescription = go False . clean
  where clean (And None a) = clean a
        clean (And a None) = clean a
        clean (Or a None) = clean a
        clean (Or None a) = clean a
        clean (And a b) =
          And (clean a)
              (clean b)
        clean (Or a b) =
          Or (clean a)
             (clean b)
        clean a = a
        go inor d =
          case d of
            Unit o -> textOpt o
            Bounded min' _ d' ->
              "[" <> go inor d' <> "]" <>
              if min' == 0
                 then "*"
                 else "+"
            And a b -> go inor a <> " " <> go inor b
            Or a b ->
              (if inor
                  then ""
                  else "(") <>
              go True a <>
              "|" <>
              go True b <>
              (if inor
                  then ""
                  else ")")
            Sequence xs ->
              T.intercalate " "
                            (map (go inor) xs)
            Wrap o d' -> textOpt o <> " " <> go inor d'
            None -> ""

-- | Make a text description of an option.
textOpt :: Option -> Text
textOpt (AnyString t) = T.map toUpper t
textOpt (Constant t) = t
textOpt (Flag t _) = "[--" <> t <> "]"
textOpt (Arg t _) = "--" <> t <> " <...>"
textOpt (Prefix t _) = "-" <> t <> "<...>"
