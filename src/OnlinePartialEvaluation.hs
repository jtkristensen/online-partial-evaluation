
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
residualProgram environment (definitions, main) = residual
  where (residual, _, _) = runRWS (partiallyEvaluate main) environment definitions

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
                let isStatic = isCanonical . snd
                let using    = local . const . fmap (second valuate)
                if     all isStatic environment
                  then using environment $ peval body
                  else do
                    let (static, dynamic) = partition isStatic environment
                    let g = f ++ hash static
                    definitionG <- lookup g <$> get
                    when (isNothing definitionG) $
                      do modify $ (:) (g, undefined)                -- future function name.
                         body' <- using static $ peval body         -- future function body.
                         modify $ filter $ (/=g) . fst              -- clean up.
                         modify $ (:) (g, (fst <$> dynamic, body')) -- define g.
                    return $ Apply g (snd <$> dynamic)
           _ -> error $ "unbound function name " ++ f
