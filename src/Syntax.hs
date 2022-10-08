
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
  | If Expression Expression Expression

data Operation
  = Equality
  | Addition
  | Subtraction
  | Multiplication

data Value
  = BooleanValue Bool
  | IntegerValue Integer
  deriving Eq

-- Abbreviations.
type Name      = String
type Arguments = [Name]
type Body      = Expression
type Main      = Expression
