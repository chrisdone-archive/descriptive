{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Descriptive parsers.

module Descriptive where

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Monoid

--------------------------------------------------------------------------------
-- Types

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

data Bound
  = NaturalBound !Integer
  | UnlimitedBound
  deriving (Show)

data Consumer d (m :: * -> *) a =
  Consumer (m (Description d)) (m (Either (Description d) a))

instance (Monoid a) => Monoid (Either (Description d) a) where
  mempty = Right mempty
  mappend x y =
    case x of
      Left e -> Left e
      Right a ->
        case y of
          Left e -> Left e
          Right b -> Right (a <> b)

instance (Monoid a,Monad f,Applicative f) => Monoid (Consumer d f a) where
  mempty = Consumer (return mempty) (pure mempty)
  mappend x y = (<>) <$> x <*> y

instance Functor f => Functor (Consumer d f) where
  fmap f (Consumer desc m) = Consumer desc (fmap (fmap f) m)

instance (Applicative m,Monad m) => Applicative (Consumer d m) where
  pure a = Consumer (return mempty) (pure (pure a))
  Consumer d f <*> Consumer d' m =
    Consumer ((<>) <$> d <*> d')
             (do rf <- f
                 case rf of
                   Left e ->
                     return (Left e)
                   Right f' ->
                     do r <- m
                        case r of
                          Left e -> return (Left e)
                          Right v ->
                            return (Right (f' v)))

--------------------------------------------------------------------------------
-- Combinators

-- | Make a consumer.
consume :: (MonadState s m)
        => m (Description d) -> (s -> m (Maybe a)) -> Consumer d m a
consume d parse =
  Consumer d
           (do s <- get
               r <- parse s
               case r of
                 Just a' ->
                   return (Right a')
                 Nothing ->
                   do d' <- d
                      return (Left d'))

-- | Wrap a consumer with another consumer.
wrap :: Monad m
     => (m (Description d) -> m (Description d))
     -> (m (Either (Description d) a) -> m (Either (Description d) b))
     -> Consumer d m a
     -> Consumer d m b
wrap updateDesc reparse (Consumer desc m) =
  Consumer (updateDesc desc)
           (reparse m)

-- | Consume many of the given consumer.
many :: MonadState s m
     => Consumer d m a -> Consumer d m [a]
many =
  wrap desc
       (\m ->
          fix (\loop !i ->
                 do restore <- get
                    r <- m
                    case r of
                      Left e
                        | i >= minb -> do put restore
                                          return (Right [])
                        | otherwise ->
                          do e' <- desc (return e)
                             return (Left e')
                      Right a ->
                        do Right as <- loop (i + 1)
                           return (Right (a : as)))
              (0 :: Integer))
  where desc = liftM (Bounded minb UnlimitedBound)
        minb = 0

-- | Compose contiguous items into one sequence.
contiguous :: (Applicative m,Monad m)
           => (a -> Consumer d m a) -> [a] -> Consumer d m [a]
contiguous p xs =
  wrap (\d ->
          do d' <- d
             return (Sequence (go d')))
       id
       (sequenceA (map p xs))
  where go (And x y) = x : go y
        go None = []
        go x = [x]
        sequenceA :: (Applicative f) => [f a] -> f [a]
        sequenceA [] = pure []
        sequenceA (x:ys) = (:) <$> x <*> sequenceA ys

--------------------------------------------------------------------------------
-- Running

-- | Run a consumer.
consumer :: Consumer d m a -> m (Either (Description d) a)
consumer (Consumer _ m) = m

-- | Describe a consumer.
describe :: Consumer d m a -> m (Description d)
describe (Consumer desc _) = desc
