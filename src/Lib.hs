{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib
    ( entry
    ) where
import Control.Applicative
import Control.Monad
import Data.Void
import qualified Data.Text as T
import Data.Text.Encoding.Base64
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
    | Matches_ String
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
    | VError String     -- Error with a message
    deriving (Eq)

-- Value Show instances

instance Show Value where
    show (VString v)        = show v
    show (VStringList v)    = show v
    show (VInt i)           = show i
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
pStatementSeparator = do space *> string "|>" <* space

pCommand :: Parser Command
pCommand =
        Base64_     <$  string "base64"
    <|> Head_       <$  string "head"
    <|> Length_     <$  string "length"
    <|> Lowercase_  <$  string "lowercase"
    <|> Matches_    <$> (string "matches" *> parens stringLiteral)
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
-- Investigate why passing multiple args causes segmentation fault: stack run hello world
-- Documentation
-- Figure out pasteboard in
-- More commands
-- Command aliases
-- Add regex commands
-- An intermediate mode that shows each transform on a new line

entry :: String -> String -> IO ()
entry inputCmd stdin = do
    case runParser pCommands "" inputCmd of
        Left e -> putStrLn $ errorBundlePretty e
        Right cs -> print $ runs cs $ VString (T.pack stdin)
