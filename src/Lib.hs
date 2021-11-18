module Lib
    ( run
    ) where
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Control.Applicative
import Control.Monad
import Data.Void
import Data.Text
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- OK, so here's what I see right now:
-- We'll start with a simple grammar. We can have parameters, but no nested expressions for now.

-- Commands can have a single param for now, 
data Command =
      CHead
    | CLowercase
    | CReverse
    | CTail
    | CTake Int
    | CWords
    | CUppercase
    deriving (Show, Eq)

data CommandType =
    TString             -- String
    | TStringList       -- [String]
    deriving (Show, Eq)

typeForCommand :: Command -> CommandType
typeForCommand c = do
    case c of 
      CHead -> TString
      CLowercase -> TString
      CReverse -> TString
      CTail -> TString
      CWords -> TStringList 
      CTake n -> TStringList
      CUppercase -> TString

runCommand :: Command -> Text -> Text
runCommand CHead t = do
    if T.null t then T.pack ""
    else singleton $ T.head t
runCommand CUppercase t = toUpper t
runCommand CLowercase t = toLower t
runCommand CReverse t = Data.Text.reverse t
-- runCommand CBase64 = encodeBase64
-- runCommand CUnbase64 = decodeBase64Lenient

-- TODO: Implement this one
-- TODO: Need to 'typecheck' here too, but do that later.
runCommands :: [Command] -> Text -> Text
runCommands (c:cs) input = do
    let output = runCommand c input
    runCommands cs output
runCommands [c] input = runCommand c input
runCommands [] input = input

--TODO: Needed?
-- data Statement = Statement { sName :: String } deriving Show

-- We can parse into tokens and attach source pos to those tokens.
-- Tokens will be in a lookup table and have a 'type' that we'll map to a haskell type. 
-- E.g. String, Int, etc. 
-- We'll use this part to typecheck between operations.

-- pInteger :: Parser 
-- pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

pTake :: Parser Command
pTake = do
    void $ string "take"
    CTake <$> parens L.decimal

pStatementSeparator :: Parser String
pStatementSeparator = do space *> string "|>" <* space

pCommand :: Parser Command
pCommand =
        CHead <$ string "head"
    <|> CLowercase <$ string "lowercase"
    <|> CTail <$ string "tail"
    <|> pTake
    <|> CWords <$ string "words"
    <|> CUppercase <$ string "uppercase"

pCommands :: Parser [Command]
pCommands = do
     pCommand `sepBy1` pStatementSeparator

-- Starting tasks:
------------------------ 
--
-- Need to capture stdin from main

-- TODO:
------------------------
--
-- Investigate why passing multiple args causes segmentation fault: stack run hello world

run :: String -> String -> IO ()
run inputCmd stdin = do
    case runParser pCommands "" inputCmd of
        Left e -> putStrLn $ errorBundlePretty e
        Right cs -> print $ runCommands cs (T.pack stdin)
    let tk = CTake 2
    print tk
    -- Parse things here
    putStrLn "someFunc"
