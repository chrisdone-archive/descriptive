{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command-line options parser.
--
-- Examples:
--
-- λ> describeList
--      ((,,,) <$>
--       constant "start" <*>
--       anyString "SERVER_NAME" <*>
--       flag "dev" "Enable dev mode?" <*>
--       arg "port" "Port to listen on")
--    And (And (And (Unit (Constant "start"))
--                  (Unit (AnyString "SERVER_NAME")))
--             (Unit (Flag "dev" "Enable dev mode?")))
--        (Unit (Arg "port" "Port to listen on"))
--
-- λ> parseList ["start","any","--port","1234","-fdev"]
--              ((,,,) <$>
--               constant "start" <*>
--               anyString "Server name" <*>
--               flag "dev" "Enable dev mode?" <*>
--               arg "port" "Port to listen on")
--    Right ("start","any",True,"1234")

module Descriptive.Options where

import Descriptive

import Control.Monad.State.Class
import Data.List
import Data.Monoid
import Data.Text (Text)

data Option
  = AnyString !Text
  | Constant !Text
  | Flag !Text !Text
  | Arg !Text !Text
  deriving (Show)

-- | Consume one argument from the argument list.
anyString :: MonadState [Text] m
          => Text -> Consumer Option m Text
anyString help =
  consume (return (Unit (AnyString help)))
          (\args ->
             case args of
               [] -> return Nothing
               (x:xs) ->
                 do put xs
                    return (Just x))

-- | Consume one argument from the argument list.
constant :: MonadState [Text] m
         => Text -> Consumer Option m Text
constant x' =
  consume (return (Unit (Constant x')))
          (\args ->
             case args of
               (x:xs) | x == x' ->
                 do put xs
                    return (Just x)
               _ -> return Nothing)

-- | Consume a short boolean flag.
flag :: MonadState [Text] m
     => Text -> Text -> Consumer Option m Bool
flag name help =
  consume (return (Unit (Flag name help)))
          (\args ->
             return (Just (elem ("-f" <> name) args)))

-- | Consume a named argument.
arg :: MonadState [Text] m
    => Text -> Text -> Consumer Option m Text
arg name help =
  consume (return (Unit (Arg name help)))
          (\args ->
             return (let indexedArgs = zip [0 :: Integer ..] args
                     in case find ((== "--" <> name) . snd) indexedArgs of
                          Nothing -> Nothing
                          Just (i,_) ->
                            case lookup (i + 1) indexedArgs of
                              Nothing -> Nothing
                              Just text ->
                                Just text))
