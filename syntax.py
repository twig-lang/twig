from dataclasses import dataclass
from typing import Optional
from enum import Enum, Flag, auto

# NOTEs:
# There is a distinction between "parameters" (what appears in function definitions),
# and "arguments" (what appears when calling a function). This is only for clarity,
# and has no effect on semantics. Also, Variable is used for names in general.

class Node:
  pass

class FunctionBody(Node): pass
class Expression(Node): pass
class Statement(Node): pass
class Type(Node): pass

class Literal(Node): pass

@dataclass
class ExpressionLiteral(Expression):
  literal: Literal

@dataclass
class Variable(Node):
  name: str

@dataclass
class StringLiteral(Literal):
  value: str

@dataclass
class IntegerLiteral(Literal):
  value: int

class Mode(Flag):
  REFERENCE = auto()
  MUTABLE = auto()

@dataclass
class Argument(Node):
  mode: Mode
  value: Expression

@dataclass
class ArgumentList(Node):
  arguments: list[Argument]

@dataclass
class Parameter(Node):
  mode: Mode
  name: Variable
  type: Type

@dataclass
class ParameterList(Node):
  parameters: list[Parameter]

@dataclass
class FunctionDefinition(Node):
  name: str
  parameters: ParameterList
  return_type: Type
  body: FunctionBody

@dataclass
class FunctionBodyExpression(FunctionBody):
  body: Expression

@dataclass
class FunctionBodyStatement(FunctionBody):
  body: Statement

@dataclass
class StatementExpression(Statement):
  expression: Expression

@dataclass
class ExpressionFunctionCall(Expression):
  callee: Expression
  arguments: ArgumentList

@dataclass
class StatementLet(Statement):
  mode: Mode
  name: Variable
  type: Optional[Type]
  value: Expression

@dataclass
class StatementBegin(Statement):
  children: list[Statement]

@dataclass
class Operator(Node):
  operator: str

@dataclass
class ExpressionBinary(Expression):
  operator: Operator
  lhs: Expression
  rhs: Expression

@dataclass
class StatementWhile(Statement):
  condition: Expression
  body: Statement

@dataclass
class StatementSet(Statement):
  binding: Variable
  operator: Optional[Operator]
  value: Expression
