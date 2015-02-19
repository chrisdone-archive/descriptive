{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Descriptive parsers.

module Descriptive
  (-- * Consuming and describing
   consume
  ,describe
   -- * Lower-level runners
  ,runConsumer
  ,runDescription
  -- * Types
  ,Description(..)
  ,Bound(..)
  ,Consumer(..)
  ,Result(..)
  -- * Combinators
  ,consumer
  ,wrap)
  where

import Control.Applicative
import Control.Applicative.QQ.Idiom
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Monoid

--------------------------------------------------------------------------------
-- Running

-- | Run a consumer.
consume :: Consumer s d Identity a -- ^ The consumer to run.
        -> s -- ^ Initial state.
        -> Result (Description d) a
consume c s = evalState (runConsumer c) s

-- | Describe a consumer.
describe :: Consumer s d Identity a -- ^ The consumer to run.
         -> s -- ^ Initial state. Can be \"empty\" if you don't use it for
              -- generating descriptions.
         -> Description d -- ^ A description and resultant state.
describe c s = evalState (runDescription c) s

-- | Run a consumer.
runConsumer :: Monad m
            => Consumer s d m a -- ^ The consumer to run.
            -> StateT s m (Result (Description d) a)
runConsumer (Consumer _ m) = m

-- | Describe a consumer.
runDescription :: Monad m
               => Consumer s d m a -- ^ The consumer to run.
               -> StateT s m (Description d) -- ^ A description and resultant state.
runDescription (Consumer desc _) = desc

--------------------------------------------------------------------------------
-- Types

-- | Description of a consumable thing.
data Description a
  = Unit !a
  | Bounded !Integer !Bound !(Description a)
  | And !(Description a) !(Description a)
  | Or !(Description a) !(Description a)
  | Sequence ![Description a]
  | Wrap a !(Description a)
  | None
  deriving (Show,Eq,Functor)

instance Monoid (Description d) where
  mempty = None
  mappend None x = x
  mappend x None = x
  mappend x y = And x y

-- | The bounds of a many-consumable thing.
data Bound
  = NaturalBound !Integer
  | UnlimitedBound
  deriving (Show,Eq)

-- | A consumer.
data Consumer s d m a =
  Consumer {consumerDesc :: StateT s m (Description d)
           ,consumerParse :: StateT s m (Result (Description d) a)}

-- | Some result.
data Result e a
  = Failed e    -- ^ The whole process failed.
  | Succeeded a -- ^ The whole process succeeded.
  | Continued e -- ^ There were errors but we continued to collect all the errors.
  deriving (Show,Eq,Ord)

instance Bifunctor Result where
  second f r =
    case r of
      Succeeded a -> Succeeded (f a)
      Failed e -> Failed e
      Continued e -> Continued e
  first f r =
    case r of
      Succeeded a -> Succeeded a
      Failed e -> Failed (f e)
      Continued e -> Continued (f e)

instance Monad m => Functor (Consumer s d m) where
  fmap f (Consumer d p) =
    Consumer d
             (do r <- p
                 case r of
                   (Failed e) ->
                     return (Failed e)
                   (Continued e) ->
                     return (Continued e)
                   (Succeeded a) ->
                     return (Succeeded (f a)))

instance Monad m => Applicative (Consumer s d m) where
  pure a =
    consumer (return mempty)
             (return (Succeeded a))
  Consumer d pf <*> Consumer d' p' =
    consumer (do e <- d
                 e' <- d'
                 return (e <> e'))
             (do mf <- pf
                 s <- get
                 ma <- p'
                 case mf of
                   Failed e ->
                     do put s
                        return (Failed e)
                   Continued e ->
                     case ma of
                       Failed e' ->
                         return (Failed e')
                       Continued e' ->
                         return (Continued (e <> e'))
                       Succeeded{} ->
                         return (Continued e)
                   Succeeded f ->
                     case ma of
                       Continued e ->
                         return (Continued e)
                       Failed e ->
                         return (Failed e)
                       Succeeded a ->
                         return (Succeeded (f a)))

instance Monad m => Alternative (Consumer s d m) where
  empty =
    consumer (return mempty)
             (return (Failed mempty))
  Consumer d p <|> Consumer d' p' =
    consumer (do d1 <- d
                 d2 <- d'
                 return (disjunct d1 d2))
             (do s <- get
                 r <- p
                 case r of
                   Continued e1 ->
                     do r' <- p'
                        case r' of
                          Failed e2 ->
                            return (Failed e2)
                          Continued e2 ->
                            return (Continued (disjunct e1 e2))
                          Succeeded a' ->
                            return (Succeeded a')
                   Failed e1 ->
                     do put s
                        r' <- p'
                        case r' of
                          Failed e2 ->
                            return (Failed (disjunct e1 e2))
                          Continued e2 ->
                            return (Continued e2)
                          Succeeded a2 ->
                            return (Succeeded a2)
                   Succeeded a1 -> return (Succeeded a1))
    where disjunct None x = x
          disjunct x None = x
          disjunct x y = Or x y
  many = sequenceHelper 0
  some = sequenceHelper 1

-- | An internal sequence maker which describes itself better than
-- regular Alternative, and is strict, not lazy.
sequenceHelper :: Monad m => Integer -> Consumer t d m a -> Consumer t d m [a]
sequenceHelper minb =
  wrap (liftM redescribe)
       (\_ p ->
          fix (\go !i as ->
                 do s <- get
                    r <- p
                    case r of
                      Succeeded a ->
                        go (i + 1)
                           (a : as)
                      Continued e ->
                        fix (\continue e' ->
                               do s' <- get
                                  r' <- p
                                  case r' of
                                    Continued e'' ->
                                      continue (e' <> e'')
                                    Succeeded{} -> continue e'
                                    Failed e''
                                      | i >= minb ->
                                        do put s'
                                           return (Continued e')
                                      | otherwise ->
                                        return (Failed (redescribe e'')))
                            e
                      Failed e
                        | i >= minb ->
                          do put s
                             return (Succeeded (reverse as))
                        | otherwise ->
                          return (Failed (redescribe e)))
              0
              [])
  where redescribe = Bounded minb UnlimitedBound

instance (Monoid a) => Monoid (Result (Description d) a) where
  mempty = Succeeded mempty
  mappend x y =
    case x of
      Failed e -> Failed e
      Continued e ->
        case y of
          Failed e' -> Failed e'
          Continued e' -> Continued (e <> e')
          Succeeded _ -> Continued e
      Succeeded a ->
        case y of
          Failed e -> Failed e
          Continued e -> Continued e
          Succeeded b -> Succeeded (a <> b)

instance (Monoid a, Monad m) => Monoid (Consumer s d m a) where
  mempty =
    consumer (return mempty)
             (return mempty)
  mappend x y = [i|(<>) x y|]

--------------------------------------------------------------------------------
-- Combinators

-- | Make a self-describing consumer.
consumer :: (StateT s m (Description d))
         -- ^ Produce description based on the state.
         -> (StateT s m (Result (Description d) a))
         -- ^ Parse the state and maybe transform it if desired.
         -> Consumer s d m a
consumer d p =
  Consumer d p

-- | Wrap a consumer with another consumer. The type looks more
-- intimidating than it actually is. The source code is trivial. It
-- simply allows for a way to transform the type of the state.
wrap :: (StateT t m (Description d) -> StateT s m (Description d))
     -- ^ Transform the description.
     -> (StateT t m (Description d) -> StateT t m (Result (Description d) a) -> StateT s m (Result (Description d) b))
     -- ^ Transform the parser. Can re-run the parser as many times as desired.
     -> Consumer t d m a
     -> Consumer s d m b
wrap redescribe reparse (Consumer d p) =
  Consumer (redescribe d)
           (reparse d p)
