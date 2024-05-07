{-# LANGUAGE OverloadedStrings #-}

module Parse (statements, Statement (..), BusVal (..)) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty, eof, sepEndBy, single, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer (space)

type Parser = Parsec Void Text

-- data BType =

data BusVal
  = BVString String
  | BVInt Int
  deriving (Show)

data Statement
  = StLet String BusVal
  | StPrint String -- TODO: allow printing of multiple variables
  deriving (Show)

ws :: Parser ()
ws = space space1 empty empty

identifier :: Parser String
identifier = some letterChar <* ws

value :: Parser String
value = some (alphaNumChar <|> single '_') <* ws

letStatement :: Parser Statement
letStatement = do
  _ <- string "let" <* ws
  name <- identifier
  _ <- char '=' <* ws
  StLet name . BVString <$> value -- TODO: parse variable type

printStatement :: Parser Statement
printStatement = do
  _ <- string "print" <* ws
  name <- between (single '(') (single ')') identifier
  return $ StPrint name

statement :: Parser Statement
statement = letStatement <|> printStatement

statements :: Parser [Statement]
statements = ws *> sepEndBy statement (ws *> char ';' <* ws) <* eof -- TODO: don't require semi-colons
