
module OnlinePartialEvaluation where

import Syntax
import Interpretation    (primitive)
import Control.Monad.RWS (RWS, ask, get, modify, local, when, runRWS)
import Data.Bifunctor    (second)
import Data.Maybe        (isNothing)
import Data.List         (partition)

type Binding     = (Name, Value)
type Environment = [Binding]
type Runtime     = RWS Environment () [FunctionDefinition]

hash :: Show a => a -> String
hash = ('$':) . show -- quick and dirty hash function {^o^}!

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

    peval (Apply f arguments) =
      do definitionF <- lookup f <$> get
         case definitionF of
           Just (parameters, body) ->
             do environment <- zip parameters <$> mapM peval arguments
                let (static, dynamic) = partition (isCanonical . snd) environment
                let run               = const . fmap (second valuate)
                if     null dynamic -- all is static
                  then local (run static) $ peval body
                  else do
                    let g       = f ++ hash static
                    definitionG <- lookup g <$> get
                    when (isNothing $ definitionG) $
                      do modify $ (:) (g, undefined) -- placeholder !
                         body' <- local (run static) $ peval body
                         modify $ (:) (g, (fst <$> dynamic, body')) . filter ((/=g) . fst)
                    return $ Apply g (snd <$> dynamic)
           _ -> error $ "unbound function name " ++ f
