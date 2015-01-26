{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command-line options parser.

module Descriptive.Options
  (-- * Existence flags
   flag
  ,switch
  -- * Text input arguments
  ,prefix
  ,arg
   -- * Token consumers
   -- $tokens
  ,anyString
  ,constant
  -- * Special control
  ,stop
  -- * Description
  ,Option(..)
  ,textDescription
  ,textOpt)
  where

import           Descriptive

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Description of a commandline option.
data Option a
  = AnyString !Text
  | Constant !Text !Text
  | Flag !Text !Text
  | Arg !Text !Text
  | Prefix !Text !Text
  | Stops
  | Stopped !a
  deriving (Show,Eq)

-- | If the consumer succeeds, stops the whole parser and returns
-- 'Stopped' immediately.
stop :: Consumer [Text] (Option a) a
     -- ^ A parser which, when it succeeds, causes the whole parser to stop.
     -> Consumer [Text] (Option a) ()
stop =
  wrap (liftM (Wrap Stops))
       (\d p ->
          do r <- p
             s <- get
             case r of
               (Failed _) ->
                 return (Succeeded ())
               (Continued e) ->
                 return (Continued e)
               (Succeeded a) ->
                 do doc <- withStateT (const s) d
                    return (Failed (Wrap (Stopped a)
                                         doc)))

-- | Consume one argument from the argument list and pops it from the
-- start of the list.
anyString :: Text -- Help for the string.
          -> Consumer [Text] (Option a) Text
anyString help =
  consumer (return d)
           (do s <- get
               case s of
                 [] -> return (Failed d)
                 (x:s') -> do put s'
                              return (Succeeded x))
  where d = Unit (AnyString help)

-- | Consume one argument from the argument list which must match the
-- given string, and also pops it off the argument list.
constant :: Text -- ^ String.
         -> Text -- ^ Description.
         -> v
         -> Consumer [Text] (Option a) v
constant x' desc v =
  consumer (return d)
           (do s <- get
               case s of
                 (x:s') | x == x' ->
                   do put s'
                      return (Succeeded v)
                 _ -> return (Failed d))
  where d = Unit (Constant x' desc)

-- | Find a value flag which must succeed. Removes it from the
-- argument list if it succeeds.
flag :: Text -- ^ Name.
     -> Text -- ^ Description.
     -> v    -- ^ Value returned when present.
     -> Consumer [Text] (Option a) v
flag name help v =
  consumer (return d)
           (do s <- get
               if elem ("--" <> name) s
                  then do put (filter (/= "--" <> name) s)
                          return (Succeeded v)
                  else return (Failed d))
  where d = Unit (Flag name help)

-- | Find a boolean flag. Always succeeds. Omission counts as
-- 'False'. Removes it from the argument list if it returns True.
switch :: Text -- ^ Name.
       -> Text -- ^ Description.
       -> Consumer [Text] (Option a) Bool
switch name help =
  flag name help True <|>
  pure False

-- | Find an argument prefixed by -X. Removes it from the argument
-- list when it succeeds.
prefix :: Text -- ^ Prefix string.
       -> Text -- ^ Description.
       -> Consumer [Text] (Option a) Text
prefix pref help =
  consumer (return d)
           (do s <- get
               case find (T.isPrefixOf ("-" <> pref)) s of
                 Nothing -> return (Failed d)
                 Just a -> do put (delete a s)
                              return (Succeeded (T.drop (T.length pref + 1) a)))
  where d = Unit (Prefix pref help)

-- | Find a named argument e.g. @--name value@. Removes it from the
-- argument list when it succeeds.
arg :: Text -- ^ Name.
    -> Text -- ^ Description.
    -> Consumer [Text] (Option a) Text
arg name help =
  consumer (return d)
           (do s <- get
               let indexedArgs =
                     zip [0 :: Integer ..] s
               case find ((== "--" <> name) . snd) indexedArgs of
                 Nothing -> return (Failed d)
                 Just (i,_) ->
                   case lookup (i + 1) indexedArgs of
                     Nothing -> return (Failed d)
                     Just text ->
                       do put (map snd (filter (\(j,_) -> j /= i && j /= i + 1) indexedArgs))
                          return (Succeeded text))
  where d = Unit (Arg name help)

-- | Make a text description of the command line options.
textDescription :: Description (Option a) -> Text
textDescription =
  go False .
  clean
  where
        go inor d =
          case d of
            Or None a -> "[" <> go inor a <> "]"
            Or a None -> "[" <> go inor a <> "]"
            Unit o -> textOpt o
            Bounded min' _ d' ->
              "[" <>
              go inor d' <>
              "]" <>
              if min' == 0
                 then "*"
                 else "+"
            And a b ->
              go inor a <>
              " " <>
              go inor b
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
            Wrap o d' ->
              textOpt o <>
              (if T.null (textOpt o)
                  then ""
                  else " ") <>
              go inor d'
            None -> ""

-- | Clean up the condition tree for single-line presentation.
clean :: Description a -> Description a
clean (And None a) = clean a
clean (And a None) = clean a
clean (Or a (Or b None)) = Or (clean a) (clean b)
clean (Or a (Or None b)) = Or (clean a) (clean b)
clean (Or None (Or a b)) = Or (clean a) (clean b)
clean (Or (Or a b) None) = Or (clean a) (clean b)
clean (Or a None) = Or (clean a) None
clean (Or None b) = Or None (clean b)
clean (And a b) =
  And (clean a)
      (clean b)
clean (Or a b) =
  Or (clean a)
     (clean b)
clean a = a

-- | Make a text description of an option.
textOpt :: (Option a) -> Text
textOpt (AnyString t) = T.map toUpper t
textOpt (Constant t _) = t
textOpt (Flag t _) = "--" <> t
textOpt (Arg t _) = "--" <> t <> " <...>"
textOpt (Prefix t _) = "-" <> t <> "<...>"
textOpt Stops = ""
textOpt (Stopped _) = ""
