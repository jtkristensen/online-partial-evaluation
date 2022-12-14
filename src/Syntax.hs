
module Syntax where

type Program
  = ([FunctionDefinition], Main)

type FunctionDefinition
  = (Name, (Arguments, Body))

data Expression
  = Constant Value
  | Variable Name
  | Apply    Name      [Expression]
  | Builtin  Operation [Expression]
  | If Condition Expression Expression
  deriving Show

data Operation
  = Equality
  | Addition
  | Subtraction
  | Multiplication
  deriving Show

data Value
  = BooleanValue Bool
  | IntegerValue Integer
  deriving (Eq, Show)

-- Abbreviations.
type Name      = String
type Arguments = [Name]
type Body      = Expression
type Main      = Expression
type Condition = Expression

class Canonical c where
  canonical   :: Value -> c
  valuate     :: c -> Value
  isCanonical :: c -> Bool

instance Canonical Value where
  canonical   = id
  valuate     = id
  isCanonical = const True

instance Canonical Expression where
  canonical                = Constant
  valuate     (Constant v) = v
  valuate     _            = error "valuation of non-canonical expressions"
  isCanonical (Constant _) = True
  isCanonical _            = False
