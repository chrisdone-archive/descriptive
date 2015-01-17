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
                (c':cs') -> (Right c',cs')
                [] -> (Left d,s))
  where d = Unit "a character"

-- | A character consumer.
char :: Char -> Consumer [Char] Text Char
char c =
  wrap (const .
        (d,))
       (\s _ p ->
          case p s of
            (Left e,s') -> (Left e,s')
            (Right c',s')
              | c' == c -> (Right c,s')
              | otherwise -> (Left d,s'))
       anyChar
  where d = Unit (T.singleton c)

-- | A string consumer.
string :: [Char] -> Consumer [Char] Text [Char]
string = sequencing . map char
