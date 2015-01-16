descriptive
=====

Self-describing consumers/parsers

There are a variety of Haskell libraries which are implementable
through a common interface: self-describing parsers:

* A formlet is a self-describing parser.
* A regular old text parser can be self-describing.
* A command-line options parser is a self-describing parser.
* A MUD command set is a self-describing parser.
* A JSON API can be a self-describing parser.

See below for some examples.

## Parsing characters

See `Descriptive.Char`.

``` haskell
λ> describeList (many (char 'k') <> string "abc")
And (Bounded 0 UnlimitedBound (Unit "k")) (Sequence [Unit "a",Unit "b",Unit "c"])

λ> parseList "kkkabc" (many (char 'k') <> string "abc")
Right "kkkabc"

λ> parseList "kkkabq" (many (char 'k') <> string "abc")
Left (Unit "c")
```

## Validating forms with named inputs

See `Descriptive.Form`.

``` haskell
λ> describeMap ((,) <$> input "username" <*> input "password")
And (Unit (Input "username")) (Unit (Input "password"))

λ> describeMap (validate "confirmed password (entered the same twice)"
                         (\(x,y) -> return (if x == y then Just y else Nothing))
                         ((,) <$> input "password" <*> input "password2"))
Wrap (Constraint "confirmed password (entered the same twice)")
     (And (Unit (Input "password")) (Unit (Input "password2")))

λ> parseMap (M.fromList [("username","chrisdone"),("password","god")])
            ((,) <$> input "username" <*> input "password")
Right ("chrisdone","god")

λ> parseMap (M.fromList [("password2","gob"),("password","god")])
            (validate "confirmed password (entered the same twice)"
                      (\(x,y) -> return (if x == y then Just y else Nothing))
                      ((,) <$> input "password" <*> input "password2"))
Left (Unit (Constraint "confirmed password (entered the same twice)"))

λ> parseMap (M.fromList [("password2","god"),("password","god")])
            (validate "confirmed password (entered the same twice)"
                      (\(x,y) -> return (if x == y then Just y else Nothing))
                      ((,) <$> input "password" <*> input "password2"))
Right "god"
```

## Validating forms with auto-generated input indexes

See `Descriptive.Formlet`.

``` haskell
λ> describeFormlet ((,) <$> indexed <*> indexed)
And (Unit (Index 0)) (Unit (Index 1))
λ> parseFormlet (M.fromList [(0,"chrisdone"),(1,"god")]) ((,) <$> indexed <*> indexed)
Right ("chrisdone","god")
λ> parseFormlet (M.fromList [(0,"chrisdone")]) ((,) <$> indexed <*> indexed)
Left (Unit (Index 2))
```

## Parsing command-line options

See `Descriptive.Options`.

``` haskell
λ> describeList
     ((,,,) <$>
      constant "start" <*>
      anyString "SERVER_NAME" <*>
      flag "dev" "Enable dev mode?" <*>
      arg "port" "Port to listen on")
   And (And (And (Unit (Constant "start"))
                 (Unit (AnyString "SERVER_NAME")))
            (Unit (Flag "dev" "Enable dev mode?")))
       (Unit (Arg "port" "Port to listen on"))

λ> parseList ["start","any","--port","1234","-fdev"]
             ((,,,) <$>
              constant "start" <*>
              anyString "Server name" <*>
              flag "dev" "Enable dev mode?" <*>
              arg "port" "Port to listen on")
   Right ("start","any",True,"1234")
```

## Self-documenting JSON parser

See `Descriptive.JSON`.

``` haskell
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

λ> parseAeson submission sample
Right (Submission {submissionToken = 123
                  ,submissionTitle = "Some title"
                  ,submissionComment = "This is good"
                  ,submissionSubreddit = 234214})

λ> describeAeson submission
Wrap (Object "Submission")
     (And (And (And (Wrap (Key "token")
                          (Unit (Integer "Submission token; see the API docs")))
                    (Wrap (Key "title")
                          (Unit (Text "Submission title"))))
               (Wrap (Key "comment")
                     (Unit (Text "Submission comment"))))
          (Wrap (Key "subreddit")
                (Unit (Integer "The ID of the subreddit"))))
```
