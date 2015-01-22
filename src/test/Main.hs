{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Test suite for ACE.

module Main where

import           Control.Applicative
import           Data.Aeson (Value(..),toJSON,object,(.=))
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import           Descriptive
import qualified Descriptive.Char as Char
import qualified Descriptive.Form as Form
import qualified Descriptive.Formlet as Formlet
import qualified Descriptive.JSON as JSON
import qualified Descriptive.Options as Options
import           Test.Hspec (Spec,it,hspec)
import qualified Test.Hspec as Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  Hspec.describe "Descriptive.Char" characters
  Hspec.describe "Descriptive.Form" form
  Hspec.describe "Descriptive.Formlet" formlet
  Hspec.describe "Descriptive.JSON" json
  Hspec.describe "Descriptive.Options" options

--------------------------------------------------------------------------------
-- Character parsing tests

characters :: Spec
characters =
  do it "describe"
        (describe (many (Char.char 'k') <>
                   Char.string "abc")
                  mempty ==
         And (Bounded 0 UnlimitedBound (Unit "k"))
             (Sequence [Unit "a"
                       ,Sequence [Unit "b",Sequence [Unit "c",Sequence []]]]))
     it "consume"
        (consume (many (Char.char 'k') <>
                  Char.string "abc")
                 "kkkabc" ==
         (Succeeded "kkkabc"))
     it "fail generic"
        (consume (many (Char.char 'k') <>
                  Char.string "abc")
                 "kkkab" ==
         (Failed (Unit "a character")))
     it "fail specific"
        (consume (many (Char.char 'k') <>
                  Char.string "abc")
                 "kkkabj" ==
         (Failed (Unit "c")))

--------------------------------------------------------------------------------
-- Form tests

form :: Spec
form =
  do it "basic describe login"
        (describe ((,) <$>
                   Form.input "username" <*>
                   Form.input "password")
                  mempty ==
         (And (Unit (Form.Input "username"))
              (Unit (Form.Input "password"))))
     it "basic describe login"
        (consume ((,) <$>
                  Form.input "username" <*>
                  Form.input "password")
                 (M.fromList [("username","chrisdone"),("password","god")]) ==
         Succeeded ("chrisdone","god"))
     it "succeeding login"
        (consume login (M.fromList [("password2","gob"),("password","gob")]) ==
         Succeeded "gob")
     it "continuing login"
        (consume login (M.fromList [("password2","gob"),("password","go")]) ==
         Continued (And (Wrap (Form.Constraint "confirmed password (entered the same twice)")
                              (And (Unit (Form.Input "password"))
                                   (Unit (Form.Input "password2"))))
                        (Unit (Form.Input "token"))))
     it "succeeding disjunction"
        (consume login
                 (M.fromList
                    [("password2","gob"),("password","go"),("token","woot")]) ==
         Succeeded "woot")
  where login =
          Form.validate
            "confirmed password (entered the same twice)"
            (\(x,y) ->
               if x == y
                  then Just y
                  else Nothing)
            ((,) <$>
             Form.input "password" <*>
             Form.input "password2") <|>
          Form.input "token"

--------------------------------------------------------------------------------
-- Formlet tests

formlet :: Spec
formlet =
  do it "basic formlet"
        (describe ((,) <$> Formlet.indexed <*> Formlet.indexed)
                  (Formlet.FormletState mempty 0) ==
         And (Unit (Formlet.Index 0))
             (Unit (Formlet.Index 1)))
     it "succeeding formlet"
        (consume ((,) <$> Formlet.indexed <*> Formlet.indexed)
                 (Formlet.FormletState (M.fromList [(0,"chrisdone"),(1,"god")])
                                       0) ==
         Succeeded ("chrisdone","god"))
     it "succeeding formlet"
        (consume ((,) <$> Formlet.indexed <*> Formlet.indexed)
                 (Formlet.FormletState (M.fromList [(0,"chrisdone")])
                                       0) ==
         Failed (Unit (Formlet.Index 1)))

--------------------------------------------------------------------------------
-- Options tests

options :: Spec
options =
  do it "describe options"
        (describe server [] ==
         And (And (And (Unit (Options.Constant "start" "cmd"))
                       (Unit (Options.AnyString "SERVER_NAME")))
                  (Or (Unit (Options.Flag "dev" "Enable dev mode?")) None))
             (Unit (Options.Arg "port" "Port to listen on")))
     it "succeeding options"
        (consume server ["start","any","--port","1234","--dev"] ==
         Succeeded ((),"any",True,"1234"))
     it "succeeding omitting port options"
        (consume server ["start","any","--port","1234"] ==
         Succeeded ((),"any",False,"1234"))
     it "failing options"
        (consume server ["start","any"] ==
         Failed (Unit (Options.Arg "port" "Port to listen on")))
  where server =
          ((,,,) <$>
           Options.constant "start" "cmd" () <*>
           Options.anyString "SERVER_NAME" <*>
           Options.switch "dev" "Enable dev mode?" <*>
           Options.arg "port" "Port to listen on")

--------------------------------------------------------------------------------
-- JSON tests

-- | Submit a URL to reddit.
data Submission =
  Submission {submissionToken :: !Integer
             ,submissionTitle :: !Text
             ,submissionComment :: !Text
             ,submissionSubreddit :: !Integer}
  deriving (Show,Eq)

submission :: Consumer Value JSON.Doc Submission
submission =
  JSON.obj "Submission"
           (Submission
             <$> JSON.key "token" (JSON.integer "Submission token; see the API docs")
             <*> JSON.key "title" (JSON.string "Submission title")
             <*> JSON.key "comment" (JSON.string "Submission comment")
             <*> JSON.key "subreddit" (JSON.integer "The ID of the subreddit"))

sample :: Value
sample =
  toJSON (object
            ["token" .= 123
            ,"title" .= "Some title"
            ,"comment" .= "This is good"
            ,"subreddit" .= 234214])

badsample :: Value
badsample =
  toJSON (object
            ["token" .= 123
            ,"title" .= "Some title"
            ,"comment" .= 123
            ,"subreddit" .= 234214])

json :: Spec
json =
  do it "describe JSON"
        (describe submission (toJSON ()) ==
         Wrap (JSON.Struct "Submission")
              (And (And (And (Wrap (JSON.Key "token")
                                   (Unit (JSON.Integer "Submission token; see the API docs")))
                             (Wrap (JSON.Key "title")
                                   (Unit (JSON.Text "Submission title"))))
                        (Wrap (JSON.Key "comment")
                              (Unit (JSON.Text "Submission comment"))))
                   (Wrap (JSON.Key "subreddit")
                         (Unit (JSON.Integer "The ID of the subreddit")))))
     it "succeeding json"
        (consume submission sample ==
         Succeeded (Submission {submissionToken = 123
                               ,submissionTitle = "Some title"
                               ,submissionComment = "This is good"
                               ,submissionSubreddit = 234214}))
     it "failing json"
        (consume submission badsample ==
         Failed (Wrap (JSON.Struct "Submission")
                      (Wrap (JSON.Key "comment")
                            (Unit (JSON.Text "Submission comment")))))
