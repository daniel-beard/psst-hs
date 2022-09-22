{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib
    ( entry
    ) where
import Control.Applicative
import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad
import Data.Monoid
import Data.Void
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Base64
import Shelly as Sh hiding (cmd)
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import Text.Megaparsec hiding (match, matchAll)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.PCRE.Light (compile)
import Text.Regex.PCRE.Light hiding (match)

-- Execution:
--
-- Each command takes a Value and returns a Value.
-- No higher order commands for now.

-- Command / Value types
------------------------------------------------------

data Command a where
    Base64_     :: Command a
    Head_       :: Command a
    Length_     :: Command a
    Lowercase_  :: Command a
    Match_      :: T.Text -> Command a
    Matches_    :: T.Text -> Command a
    Reverse_    :: Command a
    Tail_       :: Command a
    Take_       :: Int -> Command a
    UnBase64_   :: Command a
    Uppercase_  :: Command a
    Words_      :: Command a

data Value v where
    VString     :: T.Text -> Value v
    VStringList :: [T.Text] -> Value v
    VInt        :: Int -> Value v
    VBool       :: Bool -> Value v
    VError      :: String -> Value v

instance Show v => Show (Value v) where
    show (VString v) = show v
    show (VStringList l) = show l
    show (VInt i) = show i
    show (VBool b) = show b
    show (VError e) = show e

-- Running Commands
---------------------------------------------------------

-- Command, input, output
cmd :: Command c -> Value v -> Value o
-- cmd :: Command c -> (forall v. Value v -> Value v)

-- VString
-- ======================================================

cmd Base64_ (VString t)         = VString $ encodeBase64 t
cmd Head_ (VString t)           = do
    if T.null t then VString (T.pack "")
    else VString (T.singleton $ T.head t)
cmd Length_ (VString t)         = VInt $ T.length t
cmd Lowercase_ (VString t)      = VString $ T.toLower t
cmd (Match_ pattern) (VString t)   = VStringList $ t ^.. regexing (compile (encodeUtf8 pattern) []) . match
cmd (Matches_ pattern) (VString t) = VBool $ has (regexing (compile (encodeUtf8 pattern) [])) t
cmd Reverse_ (VString t)        = VString $ T.reverse t
cmd UnBase64_ (VString t)       = VString $ decodeBase64Lenient t
cmd Uppercase_ (VString t)      = VString $ T.toUpper t
cmd Tail_ (VString t)           = VString $ T.tail t
cmd (Take_ prefix) (VString t)  = VString $ T.take prefix t
cmd Words_ (VString t)          = VStringList $ T.words t

-- VStringList
-- ======================================================

cmd Base64_ (VStringList l)         = VStringList $ map encodeBase64 l
cmd Length_ (VStringList l)         = VInt $ length l
cmd Lowercase_ (VStringList l)      = VStringList $ map T.toLower l
cmd Reverse_ (VStringList l)        = VStringList $ reverse l
cmd UnBase64_ (VStringList l)       = VStringList $ map decodeBase64Lenient l
cmd Uppercase_ (VStringList l)      = VStringList $ map T.toUpper l
cmd Tail_ (VStringList l)           = VStringList $ tail l
cmd (Take_ prefix) (VStringList l)  = VStringList $ take prefix l

cmd c v = VError $ "Unsupported command: " ++ showCommand c ++ " for value type: " ++ showValue v

showCommand :: Command a -> String
showCommand a = case a of
    Base64_         -> "base64"
    Head_           -> "head"
    Length_         -> "length"
    Lowercase_      -> "lowercase"
    Match_ m        -> "match"
    Matches_ m      -> "matches"
    Reverse_        -> "reverse"
    Tail_           -> "tail" 
    Take_ i         -> "take"   
    UnBase64_       -> "unbase64"
    Uppercase_      -> "uppercase"
    Words_          -> "words" 

showValue :: Value v -> String
showValue v = case v of 
    (VString a)     -> "VString: " <> show a
    (VStringList l) -> "VStringList: " <> show l
    (VInt i)        -> "VInt: " <> show i
    (VBool b)       -> "VBool: " <> show b
    (VError e)      -> "VError: " <> show e

runs :: [Command c] -> (forall v. Value v -> Value v)
runs (c:cs) input = do
    let output = cmd c input
    case output of 
        VError e -> VError $ "Failed because :" ++ e
        _ -> runs cs output
runs [c] input = cmd c input
runs [] input = input

-- Parser 
-----------------------------------------------

type Parser = Parsec Void String

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

pStatementSeparator :: Parser String
pStatementSeparator = space *> string "|>" <* space

pCommand :: Parser (Command c)
pCommand =
        Base64_     <$  string "base64"
    <|> Head_       <$  string "head"
    <|> Length_     <$  string "length"
    <|> Lowercase_  <$  string "lowercase"
    <|> Matches_ . T.pack <$> (string "matches" *> parens stringLiteral)
    <|> Match_ . T.pack   <$> (string "match" *> parens stringLiteral)
    <|> Reverse_    <$  string "reverse"
    <|> Tail_       <$  string "tail"
    <|> Take_       <$> (string "take" *> parens L.decimal)
    <|> UnBase64_   <$  string "unbase64"
    <|> Uppercase_  <$  string "uppercase"
    <|> Words_      <$  string "words"

pCommands :: Parser [Command c]
pCommands = pCommand `sepBy1` pStatementSeparator

-- Starting tasks:
------------------------ 
--
-- Need to fix the stdin capture in main to take into account TTY

-- TODO:
------------------------
--
-- Documentation
-- Complete pasteboard in feature
-- More commands
-- Command aliases
-- Make the pipeline operator joiner syntax configurable
-- An intermediate mode that shows each transform on a new line

entry :: String -> String -> IO ()
entry inputCmd stdin = do
    istty <- queryTerminal stdInput

    -- What should the logic here be? If istty ? stdIn : pbpaste
    Sh.shelly $ Sh.verbosely $ do 
        pasteboard <- Sh.run "pbpaste" []
        liftIO $ print pasteboard

    case runParser pCommands "" inputCmd of
        Left e -> putStrLn $ errorBundlePretty e
        Right cs -> do 
            let result = runs cs $ VString (T.pack stdin)
            print $ showValue result