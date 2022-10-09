
module OnlinePartialEvaluation where

import Syntax
import Interpretation (primitive)
import Control.Monad.RWS
import Data.Bifunctor
import Data.Maybe

type Environment = [(Name, Value)]
type Runtime     = RWS Environment () [FunctionDefinition]

-- Todo, better hash function {^o^}!
hash :: Show a => a -> String
hash = ('$':) . show

residualProgram :: Environment -> (Program -> Program)
residualProgram env (defs, expr) = residual
  where (residual, _, _) = runRWS (partiallyEvaluate expr) env defs

partiallyEvaluate :: Expression -> Runtime Program
partiallyEvaluate expression =
  do residualExpression  <- peval expression
     residualDefinitions <- get
     return (residualDefinitions, residualExpression)
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
      do definitions <- get
         case lookup f definitions of
           Just (parameters, body) ->
             do let (isStatic, isDynamic)  = (isCanonical . snd, not . isStatic)
                environment  <- zip parameters <$> mapM peval arguments
                let static    = second valuate <$> filter isStatic  environment
                if     all isStatic environment
                  then local (const static) $ peval body
                  else
                  do let dynamic = filter isDynamic environment
                     let g = f ++ hash static
                     when (isNothing $ lookup g definitions) $
                       do body' <- local (const static) $ peval body
                          put $ (g, (fst <$> dynamic, body')) : definitions
                     return $ Apply g (snd <$> dynamic)
           _ -> error $ "unbound function name " ++ f

