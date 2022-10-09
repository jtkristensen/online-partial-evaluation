
module OnlinePartialEvaluation where

import Syntax
import Control.Monad.RWS

type Environment = [(Name, Value)]
type Runtime     = RWS Environment () [FunctionDefinition]

-- runPartialProgram :: Program -> Program
-- runPartialProgram (
