{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
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
  ,wrap
  ,sequencing)
  where

import Control.Applicative
import Data.Bifunctor
import Data.Function
import Data.Monoid

--------------------------------------------------------------------------------
-- Running

-- | Run a consumer.
consume :: Consumer s d a -- ^ The consumer to run.
        -> s -- ^ Initial state.
        -> Result (Description d) a
consume (Consumer _ m) = fst . m

-- | Describe a consumer.
describe :: Consumer s d a -- ^ The consumer to run.
         -> s -- ^ Initial state. Can be empty if you don't use it for
              -- generating descriptions.
         -> Description d -- ^ A description and resultant state.
describe (Consumer desc _) = fst . desc

-- | Run a consumer.
runConsumer :: Consumer s d a -- ^ The consumer to run.
            -> s -- ^ Initial state.
            -> (Result (Description d) a,s)
runConsumer (Consumer _ m) = m

-- | Describe a consumer.
runDescription :: Consumer s d a -- ^ The consumer to run.
               -> s -- ^ Initial state. Can be empty if you don't use it for
                    -- generating descriptions.
               -> (Description d,s) -- ^ A description and resultant state.
runDescription (Consumer desc _) = desc

--------------------------------------------------------------------------------
-- Types

-- | Description of a consumable thing.
data Description a
  = Unit !a
  | Bounded !Integer !Bound !(Description a)
  | And !(Description a) !(Description a)
  | Or !(Description a) !(Description a)
  | Sequence [Description a]
  | Wrap a (Description a)
  | None
  deriving (Show,Eq)

instance Monoid (Description d) where
  mempty = None
  mappend = And

-- | The bounds of a many-consumable thing.
data Bound
  = NaturalBound !Integer
  | UnlimitedBound
  deriving (Show,Eq)

-- | A consumer.
data Consumer s d a =
  Consumer {consumerDesc :: s -> (Description d,s)
           ,consumerParse :: s -> (Result (Description d) a,s)}

-- | Some result.
data Result e a
  = Failed e    -- ^ The whole process failed.
  | Succeeded a -- ^ The whole process succeeded.
  | Continued e -- ^ There were errors but we continued to collect all the errors.
  deriving (Show,Eq,Ord)

(≡) :: a -> a -> a
(≡) a b = a

--
-- first id ≡ \r -> case r of
--               Succeeded a -> Succeeded a
--               Failed e -> Failed (id e)
--               Continued e -> Continued (id e)
--          ≡ \r -> case r of
--               Succeeded a -> Succeeded a
--               Failed e -> Failed e
--               Continued e -> Continued e
--          ≡ id
--
-- second id ≡ \r -> case r of
--                      Succeeded a -> Succeeded (id a)
--                      Failed e -> Failed e
--                      Continued e -> Continued e
--           ≡ \r -> case r of
--                      Succeeded a -> Succeeded a
--                      Failed e -> Failed e
--                      Continued e -> Continued e
--           ≡ id
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

-- fmap id ≡ \(Consumer d p) ->
--              Consumer d
--               (\s ->
--                  case p s of
--                    (Failed e,s') -> (Failed e,s')
--                    (Continued e,s') -> (Continued e,s')
--                    (Succeeded a,s') -> (Succeeded (id a),s'))
--         ≡ \(Consumer d p) ->
--              Consumer d
--               (\s ->
--                  case p s of
--                    (Failed e,s') -> (Failed e,s')
--                    (Continued e,s') -> (Continued e,s')
--                    (Succeeded a,s') -> (Succeeded a,s'))
--         ≡ \(Consumer d p) ->
--              Consumer d
--               (\s -> p s)
--         ≡ \(Consumer d p) ->
--              Consumer d p
--         ≡ id
--
instance Functor (Consumer s d) where
  fmap f (Consumer d p) =
    Consumer d
             (\s ->
                case p s of
                  (Failed e,s') -> (Failed e,s')
                  (Continued e,s') -> (Continued e,s')
                  (Succeeded a,s') ->
                    (Succeeded (f a),s'))

-- identity
-- pure id <*> v = v
-- applicative_identity v =
--   (pure id <*> v) ≡
--   v
-- applicative_identity v =
--   (Consumer (\s -> (mempty,s))
--             (\s -> (Succeeded id,s)) <*>
--    v) ≡
--   v
-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer (\s ->
--                  let !(e,s') = d s
--                      !(e',s'') = d' s'
--                  in (e <> e',s''))
--               (\s ->
--                  let !(mf,s') = pf s
--                      !(ma,s'') = p' s'
--                  in case mf of
--                       Failed e -> (Failed e,s')
--                       Continued e ->
--                         case ma of
--                           Failed e' ->
--                             (Failed e',s'')
--                           Continued e' ->
--                             (Continued (e <> e'),s'')
--                           Succeeded _ ->
--                             (Continued e,s'')
--                       Succeeded f ->
--                         case ma of
--                           Continued e ->
--                             (Continued e,s'')
--                           Failed e ->
--                             (Failed e,s'')
--                           Succeeded a ->
--                             (Succeeded (f a),s'')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v
-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer (\s ->
--                  let !(e,s') = (\s -> (mempty,s)) s
--                      !(e',s'') = d' s'
--                  in (e <> e',s''))
--               (\s ->
--                  let !(mf,s') = pf s
--                      !(ma,s'') = p' s'
--                  in case mf of
--                       Failed e -> (Failed e,s')
--                       Continued e ->
--                         case ma of
--                           Failed e' ->
--                             (Failed e',s'')
--                           Continued e' ->
--                             (Continued (e <> e'),s'')
--                           Succeeded _ ->
--                             (Continued e,s'')
--                       Succeeded f ->
--                         case ma of
--                           Continued e ->
--                             (Continued e,s'')
--                           Failed e ->
--                             (Failed e,s'')
--                           Succeeded a ->
--                             (Succeeded (f a),s'')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer (\s ->
--                  let !(e,s') = (mempty,s)
--                      !(e',s'') = d' s'
--                  in (e <> e',s''))
--               (\s ->
--                  let !(mf,s') = pf s
--                      !(ma,s'') = p' s'
--                  in case mf of
--                       Failed e -> (Failed e,s')
--                       Continued e ->
--                         case ma of
--                           Failed e' ->
--                             (Failed e',s'')
--                           Continued e' ->
--                             (Continued (e <> e'),s'')
--                           Succeeded _ ->
--                             (Continued e,s'')
--                       Succeeded f ->
--                         case ma of
--                           Continued e ->
--                             (Continued e,s'')
--                           Failed e ->
--                             (Failed e,s'')
--                           Succeeded a ->
--                             (Succeeded (f a),s'')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer (\s ->
--                  let !(e',s'') = d' s
--                  in (mempty <> e',s''))
--               (\s ->
--                  let !(mf,s') = pf s
--                      !(ma,s'') = p' s'
--                  in case mf of
--                       Failed e -> (Failed e,s')
--                       Continued e ->
--                         case ma of
--                           Failed e' ->
--                             (Failed e',s'')
--                           Continued e' ->
--                             (Continued (e <> e'),s'')
--                           Succeeded _ ->
--                             (Continued e,s'')
--                       Succeeded f ->
--                         case ma of
--                           Continued e ->
--                             (Continued e,s'')
--                           Failed e ->
--                             (Failed e,s'')
--                           Succeeded a ->
--                             (Succeeded (f a),s'')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer (\s ->
--                  let !(e,s') = d' s
--                  in (e,s'))
--               (\s ->
--                  let !(mf,s') = pf s
--                      !(ma,s'') = p' s'
--                  in case mf of
--                       Failed e -> (Failed e,s')
--                       Continued e ->
--                         case ma of
--                           Failed e' ->
--                             (Failed e',s'')
--                           Continued e' ->
--                             (Continued (e <> e'),s'')
--                           Succeeded _ ->
--                             (Continued e,s'')
--                       Succeeded f ->
--                         case ma of
--                           Continued e ->
--                             (Continued e,s'')
--                           Failed e ->
--                             (Failed e,s'')
--                           Succeeded a ->
--                             (Succeeded (f a),s'')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer (\s -> d' s)
--               (\s ->
--                  let !(mf,s') = pf s
--                      !(ma,s'') = p' s'
--                  in case mf of
--                       Failed e -> (Failed e,s')
--                       Continued e ->
--                         case ma of
--                           Failed e' ->
--                             (Failed e',s'')
--                           Continued e' ->
--                             (Continued (e <> e'),s'')
--                           Succeeded _ ->
--                             (Continued e,s'')
--                       Succeeded f ->
--                         case ma of
--                           Continued e ->
--                             (Continued e,s'')
--                           Failed e ->
--                             (Failed e,s'')
--                           Succeeded a ->
--                             (Succeeded (f a),s'')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer d'
--               (\s ->
--                  let !(Succeeded f,_) = pf s
--                      !(ma,s') = p' s
--                  in case ma of
--                       Continued e ->
--                         (Continued e,s')
--                       Failed e ->
--                         (Failed e,s')
--                       Succeeded a ->
--                         (Succeeded (f a),s')))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer d'
--               (\s ->
--                  let !(Succeeded f,_) = pf s
--                  in case p' s of
--                       (Succeeded a,s') ->
--                         (Succeeded (f a),s')
--                       w -> w))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer d'
--               (\s ->
--                  case p' s of
--                    (Succeeded a,s') ->
--                      (Succeeded (id a),s')
--                    w -> w))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer d'
--               (\s ->
--                  case p' s of
--                    (Succeeded a,s') ->
--                      (Succeeded a,s')
--                    w -> w))
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\(Consumer d pf) (Consumer d' p') ->
--      Consumer d' p')
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   (\_ v' -> v')
--     (Consumer (\s -> (mempty,s))
--               (\s -> (Succeeded id,s)))
--     v ≡
--   v

-- applicative_identity v =
--   v ≡
--   v

instance Applicative (Consumer s d) where
  pure a =
    consumer (\s -> (mempty,s))
             (\s -> (Succeeded a,s))
  Consumer d pf <*> Consumer d' p' =
    consumer (\s ->
                let !(e,s') = d s
                    !(e',s'') = d' s'
                in (e <> e',s''))
             (\s ->
                let !(mf,s') = pf s
                    !(ma,s'') = p' s'
                in case mf of
                     Failed e -> (Failed e,s')
                     Continued e ->
                       case ma of
                         Failed e' ->
                           (Failed e',s'')
                         Continued e' ->
                           (Continued (e <> e'),s'')
                         Succeeded _ ->
                           (Continued e,s'')
                     Succeeded f ->
                       case ma of
                         Continued e ->
                           (Continued e,s'')
                         Failed e ->
                           (Failed e,s'')
                         Succeeded a ->
                           (Succeeded (f a),s''))

instance Alternative (Consumer s d) where
  empty =
    Consumer (\s -> (mempty,s))
             (\s -> (Failed mempty,s))
  a <|> b =
    Consumer (\s ->
                let !(d1,s') = consumerDesc a s
                    !(d2,s'') = consumerDesc b s'
                in (Or d1 d2,s''))
             (\s ->
                case consumerParse a s of
                  (Continued e1,s') ->
                    case consumerParse b s' of
                      (Failed e2,s'') ->
                        (Failed e2,s'')
                      (Continued e2,s'') ->
                        (Continued (e1 <> e2),s'')
                      (Succeeded a',s'') ->
                        (Succeeded a',s'')
                  (Failed e1,_) ->
                    case consumerParse b s of
                      (Failed e2,s') ->
                        (Failed (Or e1 e2),s')
                      (Continued e2,s'') ->
                        (Continued e2,s'')
                      (Succeeded a2,s') ->
                        (Succeeded a2,s')
                  (Succeeded a1,s') ->
                    (Succeeded a1,s'))
  some = sequenceHelper 1
  many = sequenceHelper 0

-- | An internal sequence maker which describes itself better than
-- regular Alternative, and is strict, not lazy.
sequenceHelper :: Integer -> Consumer t d a -> Consumer t d [a]
sequenceHelper minb =
  wrap (\s d -> first redescribe (d s))
       (\s _ r ->
          fix (\go !i s' as ->
                 case r s' of
                   (Succeeded a,s'') ->
                     go (i + 1)
                        s''
                        (a : as)
                   (Continued e,s'') ->
                     fix (\continue e' s''' ->
                            case r s''' of
                              (Continued e'',s'''') ->
                                continue (e' <> e'') s''''
                              (Succeeded{},s'''') ->
                                continue e' s''''
                              (Failed e'',s'''')
                                | i >= minb ->
                                  (Continued e',s''')
                                | otherwise ->
                                  (Failed (redescribe e''),s''''))
                         e
                         s''
                   (Failed e,s'')
                     | i >= minb ->
                       (Succeeded (reverse as),s')
                     | otherwise ->
                       (Failed (redescribe e),s''))
              0
              s
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

instance (Monoid a) => Monoid (Consumer s d a) where
  mempty = Consumer (\s -> (mempty,s)) (\s -> (mempty,s))
  mappend x y = (<>) <$> x <*> y

--------------------------------------------------------------------------------
-- Combinators

-- | Make a consumer.
consumer :: (s -> (Description d,s)) -- ^ Produce description based on the state.
         -> (s -> (Result (Description d) a,s)) -- ^ Parse the state and maybe transform it if desired.
         -> Consumer s d a
consumer d p =
  Consumer d p

-- | Wrap a consumer with another consumer.
wrap :: (s -> (t -> (Description d,t)) -> (Description d,s)) -- ^ Transformer the description.
     -> (s -> (t -> (Description d,t)) -> (t -> (Result (Description d) a,t)) -> (Result (Description d) b,s)) -- ^ Transform the parser. Can re-run the parser if desired.
     -> Consumer t d a -- ^ The consumer to transform.
     -> Consumer s d b -- ^ A new consumer with a potentially new state type.
wrap redescribe reparse (Consumer d p) =
  Consumer (\s -> redescribe s d)
           (\s -> reparse s d p)

-- | Compose contiguous items into one sequence. Similar to 'sequenceA'.
sequencing :: [Consumer d s a] -> Consumer d s [a]
sequencing =
  wrap (\s d ->
          first (Sequence . se)
                (d s))
       (\s _ p -> p s) .
  go
  where se (And x y) = x : se y
        se None = []
        se x = [x]
        go (x:xs) = (:) <$> x <*> sequencing xs
        go [] = mempty
