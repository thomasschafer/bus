module Lib (compile) where

import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand)

data Expr = IntLit Int | Add Expr Expr | Var String
data Stmt = Assign String Expr | Print Expr
type Program = [Stmt]

exprToLLVM :: Expr -> String
exprToLLVM (IntLit n) = show n
exprToLLVM (Add e1 e2) = "add i32 " ++ exprToLLVM e1 ++ ", " ++ exprToLLVM e2
exprToLLVM (Var x) = "%" ++ x

stmtToLLVM :: Stmt -> String
stmtToLLVM (Assign x e) =
  unlines
    [ "%" ++ x ++ " = alloca i32"
    , "store i32 " ++ exprToLLVM e ++ ", i32* %" ++ x
    ]
stmtToLLVM (Print e) = case e of
  Var x ->
    unlines
      [ "%tmp = load i32, i32* %" ++ x -- Load the integer value from the memory
      , prefix ++ " %tmp)"
      ]
  _ -> prefix ++ exprToLLVM e ++ ")"
 where
  prefix = "call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 "

programToLLVM :: Program -> String
programToLLVM stmts =
  unlines $
    [ "declare i32 @printf(i8*, ...)"
    , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\""
    , ""
    , "define i32 @main() {"
    , "entry:"
    ]
      ++ map stmtToLLVM stmts
      ++ ["ret i32 0", "}"]

compileToLLVM :: Program -> IO ()
compileToLLVM prog = do
  createDirectoryIfMissing True "dist"
  writeFile "dist/out.ll" $ programToLLVM prog

  -- Convert LLVM IR to assembly
  callCommand "llc dist/out.ll -o dist/out.s"
  -- Convert assembly to binary executable
  callCommand "clang dist/out.s -o dist/out"

  putStrLn "Compilation complete."

compile :: IO ()
compile =
  compileToLLVM
    [ Assign "x" (IntLit 99)
    , Print (Var "x")
    ]
