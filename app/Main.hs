module Main (main) where

import qualified Data.Text as T
import Go.AST (statementsToGo)
import Go.Codegen (serializeGoStatements)
import Parse (statements)
import Text.Megaparsec (errorBundlePretty, parse)

parseBusExpr :: String -> String -> IO ()
parseBusExpr inputPath outputPath = do
  input <- T.pack <$> readFile inputPath
  case parse statements inputPath input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right busAst -> do
      let goAst = statementsToGo busAst
      let goSerialised = serializeGoStatements goAst
      putStrLn goSerialised
      writeFile outputPath goSerialised

main :: IO ()
main = parseBusExpr "test/test.bus" "dist/out.go"
