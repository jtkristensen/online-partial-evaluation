
module OnlinePartialEvaluation where

import Syntax
import Interpretation (primitive)
import Control.Monad.RWS

type Environment = [(Name, Value)]
type Runtime     = RWS Environment () [FunctionDefinition]

runPartialProgram :: Program -> Program
runPartialProgram program = (fst program, residual)
  where (residual, _, _) = runRWS (partiallyEvaluate program) [] []

partiallyEvaluate :: Program -> Runtime Expression
partiallyEvaluate (definitions, mainExpression) = peval mainExpression
  where
    peval :: Expression -> Runtime Expression
    peval v@(Constant _) = return v

    peval v@(Variable x) =
      do u <- lookup x <$> ask
         return $
           case u of
             Just w -> Constant w
             _      -> v

    peval (Builtin operation arguments) =
      do parameters <- mapM peval arguments
         return $
           if     all isCanonical parameters
             then canonical $ primitive operation (valuate <$> parameters)
             else             Builtin   operation              parameters

    peval (If condition yes no) =
      do b <- peval condition
         if isCanonical b
           then peval $ if valuate b == BooleanValue True then yes else no
           else If <$> pure b <*> peval yes <*> peval no

    -- The hard part:
    peval (Apply f arguments) =
      case lookup f definitions of
        Just (parameters, body) ->
          do environment <- zip parameters <$> mapM eval arguments
             local (const environment) $ eval body
        _ -> error $ "unbound function name " ++ f


