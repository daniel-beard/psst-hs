{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib
    ( entry
    ) where
import Control.Applicative
import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad
import Data.Void
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Base64
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

-- Commands can have a single param for now, 
data Command =
      Base64_
    | Head_
    | Length_
    | Lowercase_
    | Match_ T.Text
    | Matches_ T.Text
    | Reverse_
    | Tail_
    | Take_ Int
    | UnBase64_
    | Uppercase_
    | Words_
    deriving (Show, Eq)

data Value = 
      VString T.Text     
    | VStringList [T.Text]
    | VInt Int         
    | VBool Bool
    | VError String     -- Error with a message
    deriving (Eq)

-- Value Show instances

instance Show Value where
    show (VString v)        = show v
    show (VStringList v)    = show v
    show (VInt i)           = show i
    show (VBool b)          = show b
    show (VError i)         = show i

-- Running Commands
---------------------------------------------------------

run :: Command -> Value -> Value

-- VString
-- ======================================================

run Base64_ (VString t)         = VString $ encodeBase64 t
run Head_ (VString t)           = do
    if T.null t then VString (T.pack "")
    else VString (T.singleton $ T.head t)
run Length_ (VString t)         = VInt $ T.length t
run Lowercase_ (VString t)      = VString $ T.toLower t
run (Match_ pattern) (VString t)   = VStringList $ t ^.. regexing (compile (encodeUtf8 pattern) []) . match
run (Matches_ pattern) (VString t) = VBool $ has (regexing (compile (encodeUtf8 pattern) [])) t
run Reverse_ (VString t)        = VString $ T.reverse t
run UnBase64_ (VString t)       = VString $ decodeBase64Lenient t
run Uppercase_ (VString t)      = VString $ T.toUpper t
run Tail_ (VString t)           = VString $ T.tail t
run (Take_ prefix) (VString t)  = VString $ T.take prefix t
run Words_ (VString t)          = VStringList $ T.words t

-- VStringList
-- ======================================================

run Base64_ (VStringList l)         = VStringList $ map encodeBase64 l
run Length_ (VStringList l)         = VInt $ length l
run Lowercase_ (VStringList l)      = VStringList $ map T.toLower l
run Reverse_ (VStringList l)        = VStringList $ reverse l
run UnBase64_ (VStringList l)       = VStringList $ map decodeBase64Lenient l
run Uppercase_ (VStringList l)      = VStringList $ map T.toUpper l
run Tail_ (VStringList l)           = VStringList $ tail l
run (Take_ prefix) (VStringList l)  = VStringList $ take prefix l

run c v = VError $ "Unsupported command: " ++ show c ++ " for value type: " ++ show v

runs :: [Command] -> Value -> Value
runs (c:cs) input = do
    let output = run c input
    case output of 
        VError e -> VError $ "Failed because :" ++ e
        _ -> runs cs output
runs [c] input = run c input
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

pCommand :: Parser Command
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

pCommands :: Parser [Command]
pCommands = pCommand `sepBy1` pStatementSeparator

-- Starting tasks:
------------------------ 
--
-- Need to fix the stdin capture in main to take into account TTY

-- TODO:
------------------------
--
-- Documentation
-- Figure out pasteboard in
-- More commands
-- Command aliases
-- An intermediate mode that shows each transform on a new line

entry :: String -> String -> IO ()
entry inputCmd stdin = do
    istty <- queryTerminal stdInput

    case runParser pCommands "" inputCmd of
        Left e -> putStrLn $ errorBundlePretty e
        Right cs -> print $ runs cs $ VString (T.pack stdin)
