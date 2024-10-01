module Main where

import Prelude hiding (exp)
import Syntax
import Interpretation          (result)
import OnlinePartialEvaluation (residualProgram)

dec, suc :: Name -> Expression
dec x = Builtin Subtraction [Variable x, integer 1]
suc x = Builtin Addition    [Variable x, integer 1]

integer :: Integer -> Expression
integer = Constant . IntegerValue

add, mul, exp :: FunctionDefinition
add =
  ("add", (["m", "n"],
     If (Builtin Equality [Variable "m", integer 0])
        (Variable "n")
        (Apply "add" [dec "m", suc "n"])))

mul =
  ("mul", (["m", "n"],
     If (Builtin Equality [Variable "m", integer 0])
        (integer 0)
        (Apply "add" [Variable "n", Apply "mul" [dec "m", Variable "n"]])))

exp =
  ("exp", (["x", "n"],
     If (Builtin Equality [Variable "n", integer 0])
        (integer 1)
        (Apply "mul" [Variable "x", Apply "exp" [Variable "x", dec "n"]])))


main :: IO ()
main =
  do print "-----"
     print $ result $ n $ x p
     print "-----"
     print $ result $ x $ n p
     print "-----"
     print $ result q
     print "===== Origin ========"
     putProgram $ n p
  where
    p = ([add, mul, exp], Apply "exp" $ [Variable "x", Variable "n"])
    q = ([add, mul, exp], Apply "exp" $ [integer   3 , integer   4 ])
    x = residualProgram [("x", IntegerValue 3)]
    n = residualProgram [("n", IntegerValue 4)]

putProgram :: Program -> IO ()
putProgram p =
  do mapM_ putDef (fst p)
     putExpression (snd p)
     putStrLn ""

putDef :: FunctionDefinition -> IO ()
putDef (f, (args, body)) =
  do putStr $ f ++ " "
     mapM_ (\x -> putStr $ x ++ " ") args
     putStr "=\n  "
     putExpression body
     putStrLn ""

putExpression :: Expression -> IO ()
putExpression (Constant v) = putValue v
putExpression (Variable x) = putStr x
putExpression (Apply  f es) =
  do putStr f
     case es of
       [] -> putStr "()"
       _  ->
         do putStr "("
            mapM_ (\e1 -> putExpression e1 >> putStr ", ") (init es)
            putExpression (last es)
            putStr ")"
putExpression (Builtin Equality [e1, e2]) =
  do parens e1
     putStr " == "
     parens e2
putExpression (Builtin Addition [e1, e2]) =
  do parens e1
     putStr " + "
     parens e2
putExpression (Builtin Subtraction [e1, e2]) =
  do parens e1
     putStr " - "
     parens e2
putExpression (Builtin Multiplication [e1, e2]) =
  do parens e1
     putStr " * "
     parens e2
putExpression (Builtin _ _) =
  putStr "<error>"
putExpression (If e1 e2 e3) =
  do putStr "if "
     putExpression e1
     putStr " then "
     putExpression e2
     putStr " else "
     putExpression e3

parens :: Expression -> IO ()
parens e =
  do putStr "("
     putExpression e
     putStr ")"

putValue :: Value -> IO ()
putValue (BooleanValue b) = putStr (show b)
putValue (IntegerValue n) = putStr (show n)
