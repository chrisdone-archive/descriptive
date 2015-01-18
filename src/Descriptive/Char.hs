{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Consuming form a list of characters.

module Descriptive.Char where

import           Descriptive

import           Data.Text (Text)
import qualified Data.Text as T

-- | Consume any character.
anyChar :: Consumer [Char] Text Char
anyChar =
  consumer (d,)
           (\s ->
              case s of
                (c':cs') -> (Succeeded c',cs')
                [] -> (Failed d,s))
  where d = Unit "a character"

-- | A character consumer.
char :: Char -> Consumer [Char] Text Char
char c =
  wrap (const .
        (d,))
       (\s _ p ->
          case p s of
            (Failed e,s') -> (Failed e,s')
            (Continued e,s') -> (Continued e,s')
            (Succeeded c',s')
              | c' == c -> (Succeeded c,s')
              | otherwise -> (Failed d,s'))
       anyChar
  where d = Unit (T.singleton c)

-- | A string consumer.
string :: [Char] -> Consumer [Char] Text [Char]
string = sequencing . map char
