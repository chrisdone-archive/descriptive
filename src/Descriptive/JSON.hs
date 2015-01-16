{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A JSON API which describes itself.
--
-- Examples:
--
-- λ> parseAeson (integer "token") (toJSON 123)
-- Right 123
-- λ> parseAeson (integer "token") (toJSON "abc")
-- Left (Unit (Integer "token"))
-- λ>
--
-- submission :: Consumer Doc (State Value) Submission
-- submission =
--   obj "Submission"
--       (Submission
--         <$> key "token" (integer "Submission token; see the API docs")
--         <*> key "title" (text "Submission title")
--         <*> key "comment" (text "Submission comment")
--         <*> key "subreddit" (integer "The ID of the subreddit"))
--
-- sample :: Value
-- sample =
--   toJSON (object
--             ["token" .= 123
--             ,"title" .= "Some title"
--             ,"comment" .= "This is good"
--             ,"subreddit" .= 234214])
--
-- λ> parseAeson submission sample
-- Right (Submission {submissionToken = 123
--                   ,submissionTitle = "Some title"
--                   ,submissionComment = "This is good"
--                   ,submissionSubreddit = 234214})
-- λ> describeAeson submission
-- Wrap (Object "Submission")
--      (And (And (And (Wrap (Key "token")
--                           (Unit (Integer "Submission token; see the API docs")))
--                     (Wrap (Key "title")
--                           (Unit (Text "Submission title"))))
--                (Wrap (Key "comment")
--                      (Unit (Text "Submission comment"))))
--           (Wrap (Key "subreddit")
--                 (Unit (Integer "The ID of the subreddit"))))


module Descriptive.JSON where

import Descriptive

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Aeson hiding (Object)
import Data.Aeson.Types hiding (Object)
import Data.Text (Text)

-- | Submit a URL to reddit.
data Submission =
  Submission {submissionToken :: !Integer
             ,submissionTitle :: !Text
             ,submissionComment :: !Text
             ,submissionSubreddit :: !Integer}
  deriving (Show)

-- | Description of parseable things.
data Doc
  = Integer !Text
  | Text !Text
  | Object !Text
  | Key !Text
  deriving (Show)

-- | Consume from object at the given key.
key :: MonadState Value m
    => Text -> Consumer Doc m a -> Consumer Doc m a
key k =
  wrap (liftM (Wrap doc))
       (local (\o' ->
                 case parseMaybe
                        (const (do o <- parseJSON o'
                                   o .: k))
                        () of
                   Just value -> Right value
                   Nothing -> Left (Unit doc)))
  where doc = Key k

-- | Consume an object.
obj :: (MonadState Value m)
    => Text -> Consumer Doc m a -> Consumer Doc m a
obj doc = wrap (liftM (Wrap (Object doc))) id

-- | Consume an integer.
integer :: MonadState Value m
        => Text -> Consumer Doc m Integer
integer doc =
  consume (return (Unit (Integer doc)))
          (return . parseMaybe parseJSON)

-- | Consume an text.
text :: MonadState Value m
     => Text -> Consumer Doc m Text
text doc =
  consume (return (Unit (Text doc)))
          (return . parseMaybe parseJSON)

-- | Use local state.
local :: MonadState s m
      => (s -> Either (Description d) s)
      -> m (Either (Description d) a)
      -> m (Either (Description d) a)
local f m =
  do orig <- get
     case f orig of
       Left e -> return (Left e)
       Right p ->
         do put p
            v <- m
            put orig
            return v

-- | Parse a aeson of something.
parseAeson :: Consumer Doc (State Value) a
           -> Value
           -> Either (Description Doc) a
parseAeson m = evalState (consumer m)

-- | Describe a consumer.
describeAeson :: Consumer Doc (State Value) a -> Description Doc
describeAeson (Consumer desc _) = evalState desc (toJSON ())

submission :: Consumer Doc (State Value) Submission
submission =
  obj "Submission"
      (Submission
        <$> key "token" (integer "Submission token; see the API docs")
        <*> key "title" (text "Submission title")
        <*> key "comment" (text "Submission comment")
        <*> key "subreddit" (integer "The ID of the subreddit"))

sample :: Value
sample =
  toJSON (object
            ["token" .= 123
            ,"title" .= "Some title"
            ,"comment" .= "This is good"
            ,"subreddit" .= 234214])
