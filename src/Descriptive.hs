{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Descriptive parsers.

module Descriptive where

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Data.Monoid

--------------------------------------------------------------------------------
-- Types

-- | Description of a consumable thing.
data Description a
  = Unit !a
  | Bounded !Integer !Bound !(Description a)
  | And !(Description a) !(Description a)
  | Sequence [Description a]
  | Wrap a (Description a)
  | None
  deriving (Show)

instance Monoid (Description d) where
  mempty = None
  mappend = And

-- | The bounds of a many-consumable thing.
data Bound
  = NaturalBound !Integer
  | UnlimitedBound
  deriving (Show)

-- | A consumer.
data Consumer s d a =
  Consumer (s -> (Description d,s)) (s -> (Either (Description d) a,s))

instance Functor (Consumer s d) where
  fmap f (Consumer d p) =
    Consumer d
             (\s ->
                case p s of
                  (Left e,s') -> (Left e,s')
                  (Right a,s') ->
                    (Right (f a),s'))

instance Applicative (Consumer s d) where
  pure a =
    consumer (\s -> (mempty,s))
             (\s -> (Right a,s))
  Consumer d pf <*> Consumer d' p' =
    consumer (\s ->
                let !(e,s') = d s
                    !(e',s'') = d' s'
                in (e <> e',s''))
             (\s ->
                let !(mf,s') = pf s
                    !(ma,s'') = p' s'
                in case mf of
                     Left e -> (Left e,s')
                     Right f ->
                       case ma of
                         Left e -> (Left e,s'')
                         Right a ->
                           (Right (f a),s''))

instance (Monoid a) => Monoid (Either (Description d) a) where
  mempty = Right mempty
  mappend x y =
    case x of
      Left e -> Left e
      Right a ->
        case y of
          Left e -> Left e
          Right b -> Right (a <> b)

instance (Monoid a) => Monoid (Consumer s d a) where
  mempty = Consumer (\s -> (mempty,s)) (\s -> (mempty,s))
  mappend x y = (<>) <$> x <*> y

--------------------------------------------------------------------------------
-- Combinators

-- | Make a consumer.
consumer :: (s -> (Description d,s)) -> (s -> (Either (Description d) a,s)) -> Consumer s d a
consumer d p =
  Consumer d p

-- | Wrap a consumer with another consumer.
wrap :: (s -> (t -> (Description d,t)) -> (Description d,s))
     -> (s -> (t -> (Description d,t)) -> (t -> (Either (Description d) a,t)) -> (Either (Description d) b,s))
     -> Consumer t d a
     -> Consumer s d b
wrap redescribe reparse (Consumer d p) =
  Consumer (\s -> redescribe s d)
           (\s -> reparse s d p)

-- | Consume many of the given consumer.
zeroOrMore :: Consumer s d a -> Consumer s d [a]
zeroOrMore =
  wrap (\s d -> first redescribe (d s))
       (\s _ r ->
          fix (\go !i s' as ->
                 case r s' of
                   (Right a,s'') ->
                     go (i + 1)
                        s''
                        (a : as)
                   (Left e,s'')
                     | i >= minb ->
                       (Right (reverse as),s')
                     | otherwise ->
                       (Left (redescribe e),s''))
              0
              s
              [])
  where redescribe = Bounded minb UnlimitedBound
        minb = 0

-- | Compose contiguous items into one sequence.
sequencing :: [Consumer d s a] -> Consumer d s [a]
sequencing (x:xs) = (:) <$> x <*> sequencing xs
sequencing [] = mempty

--------------------------------------------------------------------------------
-- Running

-- | Run a consumer.
consume :: Consumer s d a -> s -> (Either (Description d) a,s)
consume (Consumer _ m) = m

-- | Describe a consumer.
describe :: Consumer s d a -> s -> (Description d,s)
describe (Consumer desc _) = desc
