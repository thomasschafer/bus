{-# LANGUAGE OverloadedStrings #-}

module Parse (parseBusExpr) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty, eof, errorBundlePretty, parse, sepEndBy, single, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer (space)

type Parser = Parsec Void Text

data Statement
  = StLet String String
  | StPrint String
  deriving (Show)

sc :: Parser ()
sc = space space1 empty empty

identifier :: Parser String
identifier = some letterChar <* sc

value :: Parser String
value = some (alphaNumChar <|> single '_') <* sc

letStatement :: Parser Statement
letStatement = do
  _ <- string "let" <* sc
  name <- identifier
  _ <- char '=' <* sc
  StLet name <$> value

printStatement :: Parser Statement
printStatement = do
  _ <- string "print" <* sc
  name <- between (single '(') (single ')') identifier
  return $ StPrint name

statement :: Parser Statement
statement = letStatement <|> printStatement

statements :: Parser [Statement]
statements = sepEndBy statement (sc *> char ';' <* sc) <* eof -- TODO: don't require semi-colons

parseBusExpr :: String -> IO ()
parseBusExpr filePath = do
  input <- T.pack <$> readFile filePath
  case parse statements filePath input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> print result
