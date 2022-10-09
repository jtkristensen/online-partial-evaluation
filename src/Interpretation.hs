
module Interpretation where

import Syntax
import Control.Monad.Reader

type Environment = [(Name, Value)]
type Runtime     = Reader Environment

runProgram :: Program -> Value
runProgram program = runReader (evaluate program) []

evaluate :: Program -> Runtime Value
evaluate (definitions, mainExpression) = eval mainExpression
  where
    eval :: Expression -> Runtime Value
    eval (Constant v) = return v

    eval (Variable x) =
      do u <- lookup x <$> ask
         case u of
           Just v -> return v
           _      -> error $ "unbound variable " ++ x

    eval (Apply f arguments) =
      case lookup f definitions of
        Just (parameters, body) ->
          do environment <- zip parameters <$> mapM eval arguments
             local (environment++) $ eval body
        _ -> error $ "unbound function name " ++ f

    eval (Builtin operation arguments) =
      do parameters <- mapM eval arguments
         return $ primitive operation parameters

    eval (If condition yes no) =
      do b <- eval condition
         eval $ if b == BooleanValue True then yes else no

    primitive :: Operation -> [Value] -> Value
    primitive operation args =
      case (operation, args) of
        (Equality,       [u             , v             ]) -> BooleanValue $ u == v
        (Addition,       [IntegerValue m, IntegerValue n]) -> IntegerValue $ m  + n
        (Subtraction,    [IntegerValue m, IntegerValue n]) -> IntegerValue $ m  - n
        (Multiplication, [IntegerValue m, IntegerValue n]) -> IntegerValue $ m  * n
        _                                                  -> error "invalid operation"
