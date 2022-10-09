module Main where

import Prelude hiding (exp)
import Syntax
import Interpretation          (runProgram)
import OnlinePartialEvaluation (runPartialProgram)

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
  print $ runProgram $ ([add, mul, exp], Apply "exp" $ [integer 3, integer 4])
