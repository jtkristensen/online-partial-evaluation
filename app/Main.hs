module Main where

import Syntax
import Interpretation

dec, suc :: Name -> Expression
dec x = Builtin Subtraction [Variable x, integer 1]
suc x = Builtin Addition    [Variable x, integer 1]

integer :: Integer -> Expression
integer = Constant . IntegerValue

add =
  ("add", (["m", "n"],
     If (Builtin Equality [Variable "m", integer 0])
        (Variable "n")
        (Apply "add" [dec "m", suc "n"])))


main :: IO ()
main =
  print $ runProgram $ ([add], Apply "add" $ [integer 2, integer 2])
