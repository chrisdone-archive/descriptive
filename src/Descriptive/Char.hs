{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Consuming form a list of characters.
--
-- Examples:
--
-- λ> describeList (many (char 'k') <> string "abc")
-- And (Bounded 0 UnlimitedBound (Unit "k")) (Sequence [Unit "a",Unit "b",Unit "c"])
--
-- λ> parseList "kkkabc" (many (char 'k') <> string "abc")
-- Right "kkkabc"
--
-- λ> parseList "kkkabq" (many (char 'k') <> string "abc")
-- Left (Unit "c")

module Descriptive.Char where

import           Descriptive

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T

-- | Consume any character.
anyChar :: MonadState [Char] m
        => Consumer Text m Char
anyChar =
  consume (return (Unit "a character"))
          (\cs ->
             case cs of
               (c':cs') ->
                 do put cs'
                    return (Just c')
               _ -> return Nothing)

-- | A character consumer.
char :: MonadState [Char] m
     => Char -> Consumer Text m Char
char c =
  wrap (const (return desc))
       (\m ->
          do r <- m
             case r of
               Left e -> return (Left e)
               Right c'
                 | c' == c -> return (Right c)
                 | otherwise ->
                   return (Left desc))
       anyChar
  where desc = Unit (T.singleton c)

-- | A string consumer.
string :: (MonadState [Char] m,Applicative m)
       => [Char] -> Consumer Text m [Char]
string = contiguous char
