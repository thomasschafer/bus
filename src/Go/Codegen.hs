module Go.Codegen (serializeGoStatements) where

import Data.List (intercalate)
import Go.AST (GoStatement (..), GoVal (..))

serializeGoStatement :: GoStatement -> String
serializeGoStatement (GSVarDef name (GVBool b)) = name ++ " := " ++ show b
serializeGoStatement (GSVarDef name (GVString s)) = name ++ " := " ++ show s
serializeGoStatement (GSVarDef name (GVInt i)) = name ++ " := " ++ show i
serializeGoStatement (GSFuncDef funcName args statements) =
  "func " ++ funcName ++ "() {\n" ++ unlines (map (('\t' :) . serializeGoStatement) statements) ++ "}" -- TODO: use args
serializeGoStatement (GSImport name) = "import \"" ++ name ++ "\"\n" -- TODO: group imports
serializeGoStatement (GSFuncCall name args) = name ++ "(" ++ intercalate ", " args ++ ")"
serializeGoStatement other = error $ "Didn't handle " ++ show other -- TODO: implement remainder

serializeGoStatements :: [GoStatement] -> String
serializeGoStatements = intercalate "\n" . ("package main\n" :) . map serializeGoStatement
