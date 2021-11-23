module Lib
    ( run
    ) where
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Control.Applicative
import Control.Monad
import Data.Void
import qualified Data.Text as T
import Data.Text.Encoding.Base64
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- OK, so here's what I see right now:
-- We'll start with a simple grammar. We can have parameters, but no nested expressions for now.

-- Command / Value types
------------------------------------------------------

-- Commands can have a single param for now, 
data Command =
      CBase64
    | CHead
    | CLength
    | CLowercase
    | CReverse
    | CTail
    | CTake Int
    | CUnBase64
    | CUppercase
    | CWords
    deriving (Show, Eq)

data Value = 
      VString T.Text     
    | VStringList [T.Text]
    | VInt Int         
    | VError String     -- Error with a message
    deriving (Show, Eq)

-- Running Commands
---------------------------------------------------------

runCommand :: Command -> Value -> Value

runCommand CBase64 (VString t) = VString $ encodeBase64 t
runCommand CBase64 (VStringList l) = VStringList $ map encodeBase64 l
runCommand CBase64 _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CHead (VString t) = do
    if T.null t then VString (T.pack "")
    else VString (T.singleton $ T.head t)
runCommand CHead _ = VError "Unexpected type, expected {VString}"

runCommand CLength (VString t) = VInt $ T.length t
runCommand CLength (VStringList l) = VInt $ length l
runCommand CLength _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CLowercase (VString t) = VString $ T.toLower t
runCommand CLowercase (VStringList l) = VStringList $ map T.toLower l
runCommand CLowercase _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CReverse (VString t) = VString $ T.reverse t
runCommand CReverse (VStringList l) = VStringList $ map T.reverse l
runCommand CReverse _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CUnBase64 (VString t) = VString $ decodeBase64Lenient t
runCommand CUnBase64 (VStringList l) = VStringList $ map decodeBase64Lenient l
runCommand CUnBase64 _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CUppercase (VString t) = VString $ T.toUpper t
runCommand CUppercase (VStringList l) = VStringList $ map T.toUpper l
runCommand CUppercase _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CTail (VString t) = VString $ T.tail t
runCommand CTail (VStringList l) = VStringList $ map T.tail l
runCommand CTail _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand (CTake prefix) (VString t) = VString $ T.take prefix t
runCommand (CTake prefix) (VStringList l) = VStringList $ map (T.take prefix) l
runCommand (CTake prefix) _ = VError "Unexpected type, expected {VString, VStringList}"

runCommand CWords (VString t) = VStringList $ T.words t
runCommand CWords _ = VError "Unexpected type, expected {VString}"

runCommand c v = VError $ "Unsupported command: " ++ show c ++ " or value type: " ++ show v

runCommands :: [Command] -> Value -> Value
runCommands (c:cs) input = do
    let output = runCommand c input
    case output of 
        VError e -> VError $ "Failed because :" ++ e
        _ -> runCommands cs output
runCommands [c] input = runCommand c input
runCommands [] input = input

-- Parser 
-----------------------------------------------

type Parser = Parsec Void String

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
        CBase64     <$ string "base64"
    <|> CHead       <$ string "head"
    <|> CLength     <$ string "length"
    <|> CLowercase  <$ string "lowercase"
    <|> CReverse    <$ string "reverse"
    <|> CTail       <$ string "tail"
    <|> pTake
    <|> CUnBase64   <$ string "unbase64"
    <|> CUppercase  <$ string "uppercase"
    <|> CWords      <$ string "words"

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
-- Better Show instances to strip top level var descriptions
-- Add regex commands

run :: String -> String -> IO ()
run inputCmd stdin = do
    case runParser pCommands "" inputCmd of
        Left e -> putStrLn $ errorBundlePretty e
        Right cs -> print $ runCommands cs $ VString (T.pack stdin)
