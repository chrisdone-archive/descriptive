{-# LANGUAGE FlexibleContexts #-}

-- | Validating form with named inputs.
--
-- Examples:
--
-- λ> describeMap ((,) <$> input "username" <*> input "password")
-- And (Unit (Input "username")) (Unit (Input "password"))

-- λ> describeMap (validate "confirmed password (entered the same twice)"
--                          (\(x,y) -> return (if x == y then Just y else Nothing))
--                          ((,) <$> input "password" <*> input "password2"))
-- Wrap (Constraint "confirmed password (entered the same twice)")
--      (And (Unit (Input "password")) (Unit (Input "password2")))
--
-- λ> parseMap (M.fromList [("username","chrisdone"),("password","god")])
--             ((,) <$> input "username" <*> input "password")
-- Right ("chrisdone","god")

-- λ> parseMap (M.fromList [("password2","gob"),("password","god")])
--             (validate "confirmed password (entered the same twice)"
--                       (\(x,y) -> return (if x == y then Just y else Nothing))
--                       ((,) <$> input "password" <*> input "password2"))
-- Left (Unit (Constraint "confirmed password (entered the same twice)"))
--
-- λ> parseMap (M.fromList [("password2","god"),("password","god")])
--             (validate "confirmed password (entered the same twice)"
--                       (\(x,y) -> return (if x == y then Just y else Nothing))
--                       ((,) <$> input "password" <*> input "password2"))
-- Right "god"


module Descriptive.Form where

import           Descriptive

import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)

data Form
  = Input !Text
  | Constraint !Text
  deriving (Show)

-- | Consume any character.
input :: MonadState (Map Text Text) m
      => Text -> Consumer Form m Text
input name =
  consume (return (Unit (Input name)))
          (\m ->
             return (case M.lookup name m of
                       Nothing -> Nothing
                       Just value -> Just value))

-- | Validate a form input with a description of what's required.
validate :: (Monad m)
         => Text -> (a -> m (Maybe b)) -> Consumer Form m a -> Consumer Form m b
validate d f =
  wrap (liftM (Wrap (Constraint d)))
       (\m ->
          do r <- m
             case r of
               Left e -> return (Left e)
               Right k ->
                 do r' <- f k
                    case r' of
                      Nothing ->
                        return (Left (Unit (Constraint d)))
                      Just o -> return (Right o))

-- | Parse a list of something.
parseMap :: Map k v -> Consumer d (State (Map k v)) a -> Either (Description d) a
parseMap st m = evalState (consumer m) st

-- | Describe a consumer.
describeMap :: Ord k => Consumer d (State (Map k v)) a -> Description d
describeMap (Consumer desc _) = evalState desc mempty
