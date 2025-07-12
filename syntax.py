from dataclasses import dataclass
from typing import *
from enum import Enum, Flag, auto

# NOTEs:
# There is a distinction between "parameters" (what appears in function definitions),
# and "arguments" (what appears when calling a function). This is only for clarity,
# and has no effect on semantics.

class Node:
  pass

class FunctionBody(Node): pass
class Expression(Node): pass
class Statement(Node): pass

class ExpressionLiteral(Expression): pass

@dataclass
class Variable(Node):
  name: str

@dataclass
class StringLiteral(ExpressionLiteral):
  value: str

@dataclass
class IntegerLiteral(ExpressionLiteral):
  value: int

class Mode(Flag):
  REFERENCE = auto()
  MUTABLE = auto()

@dataclass
class Argument(Node):
  mode: Mode
  value: Node

@dataclass
class ArgumentList(Node):
  arguments: list[Argument]

@dataclass
class Parameter(Node):
  mode: Mode
  name: str
  type: Node

@dataclass
class ParameterList(Node):
  parameters: list[Parameter]

@dataclass
class FunctionDefinition(Node):
  name: str
  parameters: ParameterList
  return_type: Node
  body: Optional[Node]

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
