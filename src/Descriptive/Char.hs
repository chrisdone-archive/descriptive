{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Consuming form a list of characters.

module Descriptive.Char where

import           Data.Traversable
import           Descriptive

import           Control.Monad.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T

-- | Consume any character.
anyChar :: Consumer [Char] Text Char
anyChar =
  consumer (return d)
           (do s <- get
               case s of
                 (c':cs') -> do put cs'
                                return (Succeeded c')
                 [] -> return (Failed d))
  where d = Unit "a character"

-- | A character consumer.
char :: Char -> Consumer [Char] Text Char
char c =
  wrap (liftM (const d))
       (\_ p ->
          do r <- p
             return (case r of
                       (Failed e) -> Failed e
                       (Continued e) ->
                         Continued e
                       (Succeeded c')
                         | c' == c -> Succeeded c
                         | otherwise -> Failed d))
       anyChar
  where d = Unit (T.singleton c)

-- | A string consumer.
string :: [Char] -> Consumer [Char] Text [Char]
string =
  wrap (liftM (Sequence . flattenAnds))
       (\_ p -> p) .
  sequenceA . map char
  where flattenAnds (And x y) = flattenAnds x ++ flattenAnds y
        flattenAnds x = [x]
