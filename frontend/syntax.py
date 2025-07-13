from dataclasses import dataclass
from typing import Optional
from enum import Enum, Flag, auto

# NOTEs:
# There is a distinction between "parameters" (what appears in function definitions),
# and "arguments" (what appears when calling a function). This is only for clarity,
# and has no effect on semantics.


class Node:
    pass


class FunctionBody(Node):
    pass


class Expression(Node):
    pass


class Statement(Node):
    pass


class Type(Node):
    pass


class Literal(Node):
    pass


@dataclass
class ExpressionLiteral(Expression):
    literal: Literal


@dataclass
class Name(Node):
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
    key: Optional[Name]
    value: Expression


@dataclass
class ArgumentList(Node):
    arguments: list[Argument]


@dataclass
class Parameter(Node):
    mode: Mode
    name: Name
    key: Optional[Name]
    type: Type
    default: Optional[Expression]


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
class LetBinding(Node):
    mode: Mode
    name: Name
    type: Optional[Type]
    value: Expression


@dataclass
class StatementLet(Statement):
    bindings: list[LetBinding]


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
class SetBinding(Node):
    lvalue: Name
    operator: Optional[Operator]
    rvalue: Expression


@dataclass
class StatementSet(Statement):
    bindings: list[SetBinding]


@dataclass
class StatementReturn(Statement):
    value: Optional[Expression]


@dataclass
class StatementIf(Statement):
    condition: Expression
    taken: Statement
    not_taken: Optional[Statement]


@dataclass
class TypeNamed(Type):
    name: Name


class Path(Node):
    pass


class WithPath(Path):
    pass


@dataclass
class PathNamed(Path):
    name: Name


@dataclass
class WithPathMembers(WithPath):
    members: list[WithPath]


@dataclass
class PathSub(Path):
    parent: Path
    child: Path


@dataclass
class StatementWith(Statement):
    imports: bool
    path: WithPath


@dataclass
class ExpressionSubscriptCall(Expression):
    callee: Expression
    arguments: ArgumentList


@dataclass
class SubscriptDefinition(Node):
    name: str
    mode: Mode
    parameters: ParameterList
    return_type: Type
    body: FunctionBody


@dataclass
class StatementYield(Statement):
    mode: Mode
    value: Expression


@dataclass
class Import(Node):
    path: Path


@dataclass
class TypeDefinition(Node):
    name: Name
    type: Type


class TypeUnit(Type):
    pass
