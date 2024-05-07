module Go.AST where

import Control.Arrow (Arrow (first))
import Data.List (nub, partition)
import Parse (BusVal (..), Statement (..))

data GoType -- TODO: we might not need all of these
  = GTBool
  | GTString
  | GTInt
  | GTInt8
  | GTInt16
  | GTInt32
  | GTInt64
  | GTUint
  | GTUint8
  | GTUint16
  | GTUint32
  | GTUint64
  | GTUintptr
  | GTByte
  | GTRune
  | GTFloat32
  | GTFloat64
  | GTComplex64
  | GTComplex128
  deriving (Show)

data GoVal
  = GVBool Bool
  | GVString String
  | GVInt Int
  deriving (Show)

data GoStatement
  = GSVarDef String GoVal
  | GSFuncCall String [String]
  | GSImport String
  | GSFuncDef String [(String, GoType)] [GoStatement]
  | GSStructDef String [(String, GoType)]
  | GSInterfaceDef String [(String, GoType)]
  deriving (Show)

-- TODO: add more above

statementToGo :: Statement -> ([String], GoStatement) -- first element is imports
statementToGo (StLet name (BVString val)) = ([], GSVarDef name (GVString val))
statementToGo (StLet name (BVInt val)) = ([], GSVarDef name (GVInt val))
statementToGo (StPrint varName) = (["fmt"], GSFuncCall "fmt.Println" [varName])

statementsToGo :: [Statement] -> [GoStatement]
statementsToGo statements = map GSImport imports ++ rest ++ [GSFuncDef "main" [] topLevel]
 where
  (imports, nonImports) = first (nub . concat) . unzip $ map statementToGo statements
  (topLevel, rest) = partition isTopLevel nonImports

  isTopLevel = \case
    GSFuncDef{} -> False
    GSStructDef{} -> False
    GSInterfaceDef{} -> False
    _ -> True
