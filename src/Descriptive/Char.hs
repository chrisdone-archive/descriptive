{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

-- | Consuming form a list of characters.

module Descriptive.Char where

#if __GLASGOW_HASKELL__ < 802
import           Data.Traversable
#endif
import           Descriptive

import           Control.Monad.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T

-- | Consume any character.
anyChar :: Monad m => Consumer [Char] Text m Char
anyChar =
  consumer (return d)
           (do s <- get
               case s of
                 (c':cs') -> do put cs'
                                return (Succeeded c')
                 [] -> return (Failed d))
  where d = Unit "a character"

-- | A character consumer.
char :: Monad m => Char -> Consumer [Char] Text m Char
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
string :: Monad m => [Char] -> Consumer [Char] Text m [Char]
string =
  wrap (liftM (Sequence . flattenAnds))
       (\_ p -> p) .
  sequenceA . map char
  where flattenAnds (And x y) = flattenAnds x ++ flattenAnds y
        flattenAnds x = [x]
