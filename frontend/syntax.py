from dataclasses import dataclass
from typing import Optional
from enum import Enum, Flag, auto

# NOTEs:
# There is a distinction between "parameters" (what appears in function definitions),
# and "arguments" (what appears when calling a function). This is only for clarity,
# and has no effect on semantics.


class Node:
    pass


# A parsing error.
class Error(Node):
    def __repr__(self):
        return "syntax.Error"


class Pattern(Node):
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
class StringLiteral(Literal):
    value: str


@dataclass
class IntegerLiteral(Literal):
    value: int


# [ '&' ] [ 'mut' ]
class Mode(Flag):
    REFERENCE = auto()
    MUTABLE = auto()


# [ ':' name ] mode expression
@dataclass
class Argument(Node):
    key: Optional[str]
    mode: Mode
    value: Expression


# [ argument { ',' argument } ]
@dataclass
class ArgumentList(Node):
    arguments: list[Argument]


# mode name [ name ':' ] type [ '=' expression ]
@dataclass
class Parameter(Node):
    mode: Mode
    name: str
    key: Optional[str]
    type: Type
    default: Optional[Expression]


# [ parameter { ',' parameter } ]
@dataclass
class ParameterList(Node):
    parameters: list[Parameter]


# 'function' ['unsafe'] name [ '(' parameter-list ')' ] [ ':' type ] function-body
@dataclass
class FunctionDefinition(Node):
    is_unsafe: bool
    name: str
    parameters: ParameterList
    return_type: Type
    body: FunctionBody


# '=' expression ';'
@dataclass
class FunctionBodyExpression(FunctionBody):
    body: Expression


# statement
@dataclass
class FunctionBodyStatement(FunctionBody):
    body: Statement


# ';'
class FunctionBodyDeclaration(FunctionBody):
    def __repr__(self):
        return "syntax.FunctionBodyDeclaration"


# expression
@dataclass
class StatementExpression(Statement):
    expression: Expression


# expression '(' argument-list ')'
@dataclass
class ExpressionFunctionCall(Expression):
    callee: Expression
    arguments: ArgumentList


# name [ ':' type ] '=' top-expression
@dataclass
class LetBinding(Node):
    name: str
    type: Optional[Type]
    value: Expression


# [ 'while' | 'if' ]
class BindingKind(Enum):
    VALUE = "value"
    IF = "if"
    WHILE = "while"


# 'let' [ kind ] bindings [ 'in' statement ]
# NOTE: the 'in' is required for let-if and let-while bindings,
# and optional for "regular" bindings.
@dataclass
class StatementLet(Statement):
    kind: BindingKind
    bindings: list[LetBinding]
    body: Optional[Statement]


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
    lvalue: str
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
    name: str


class Path(Node):
    pass


class WithPath(Path):
    pass


@dataclass
class PathNamed(Path):
    name: str


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


# 'subscript' ['unsafe'] ['if' | 'while'] mode name [parameter-list] [':' type] body
@dataclass
class SubscriptDefinition(Node):
    is_unsafe: bool
    kind: BindingKind
    mode: Mode
    name: str
    parameters: ParameterList
    return_type: Type
    body: FunctionBody


# 'yield' mode expression ';'
@dataclass
class StatementYield(Statement):
    mode: Mode
    value: Expression


# 'import' path ';'
@dataclass
class Import(Node):
    path: Path


# 'type' name [ '=' type ] ';'
@dataclass
class TypeDefinition(Node):
    name: str
    type: Optional[Type]


class TypeUnit(Type):
    def __repr__(self):
        return "TypeUnit"


# [ name ':' ] path
@dataclass
class ModuleArgument(Node):
    key: Optional[str]
    value: Path


# path '(' { <module argument> }, ')'
@dataclass
class PathCall(Path):
    callee: Path
    arguments: list[ModuleArgument]


# name ':' type
@dataclass
class Member(Node):
    name: str
    type: Type


# 'record' { member },
@dataclass
class TypeRecord(Type):
    members: list[Member]


# 'union' { member },
@dataclass
class TypeUnion(Type):
    members: list[Member]


# '(' type { ',' type } ')'
@dataclass
class TypeTuple(Type):
    members: list[Type]


# 'enum' { name },
@dataclass
class TypeEnum(Type):
    names: list[str]


# name [ '(' { type }, ')' ]
@dataclass
class Variant(Node):
    name: str
    arguments: list[Type]


# 'variant' { variant },
@dataclass
class TypeVariant(Type):
    cases: list[Variant]


# path
@dataclass
class PatternNamed(Pattern):
    name: Path


# [ name ':' ] pattern
@dataclass
class ConstructorArgument(Node):
    key: Optional[str]
    value: Pattern


# pattern '(' { constructor-argument }, ')'
@dataclass
class PatternConstructor(Pattern):
    constructor: Pattern
    arguments: [ConstructorArgument]


# 'case' pattern ':' stmt
# 'else' : stmt (NOTE: then, pattern is None)
@dataclass
class Case(Node):
    pattern: Optional[Pattern]
    body: Statement


# 'match' expression 'begin' { case } 'end'
# 'match' expression case
@dataclass
class StatementMatch(Statement):
    checked: Expression
    cases: list[Case]


# 'pass' ';'
class StatementPass(Statement):
    def __repr__(self):
        return "syntax.StatementPass"


# unary-operator expression
@dataclass
class ExpressionUnary(Expression):
    operator: Operator
    rhs: Expression


# expression ':' type
@dataclass
class ExpressionCast(Expression):
    lhs: Expression
    type: Type


# 'extern' string name ':' type ';'
@dataclass
class ExternVariable(Node):
    abi: str
    name: str
    type: Type


# 'extern' 'function' string name [ parameter-list ] ':' type ';'
@dataclass
class ExternFunction(Node):
    abi: str
    name: str
    parameters: ParameterList
    returns: Type


class ModuleExpression(Node):
    pass


# path
@dataclass
class ModulePath(ModuleExpression):
    path: Path


# 'begin' { toplevel } 'end'
@dataclass
class ModuleBlock(ModuleExpression):
    definitions: list[Node]


# module-expression '+' module-expression
@dataclass
class ModuleJoin(ModuleExpression):
    lhs: ModuleExpression
    rhs: ModuleExpression


# name [ name ] ':' module-expression
# 'type' [ name ] name
@dataclass
class ModuleParameter(Node):
    is_type: bool
    name: str
    key: Optional[str]
    sig: Optional[ModuleExpression]


# 'module' name [ ':' module-expression ] [ '!' mod-par-list  ] '=' module-expression ';'
@dataclass
class ModuleDefinition(Node):
    name: str
    type: Optional[ModuleExpression]
    parameters: list[ModuleParameter]
    value: ModuleExpression


# 'module' 'type' name [ '!' mod-par-list ] '=' module-expression ';'
@dataclass
class ModuleType(Node):
    name: str
    parameters: list[ModuleParameter]
    value: ModuleExpression


# 'module' [ '!' mod-par-list ] ';'
@dataclass
class ModuleGlobal(Node):
    parameters: list[ModuleParameter]


# '(' [ mod-par { ',' mod-par }  ] ')'
# class ModuleParameterList


# '(' ')'
class ExpressionUnit(Expression):
    def __repr__(self):
        return "syntax.ExpressionUnit"


# expression '.' name
@dataclass
class ExpressionMember(Expression):
    lhs: Expression
    member: str


# 'unsafe' statement
@dataclass
class StatementUnsafe(Statement):
    inner: Statement


# 'unsafe' expression
@dataclass
class ExpressionUnsafe(Expression):
    inner: Expression


# mode expression
@dataclass
class ModedExpression(Node):
    mode: Mode
    value: Expression


# 'with' module-expression ';'
@dataclass
class StatementWith(Statement):
    module: ModuleExpression


# 'module' name [':' module-expression] '=' module-expression ';'
@dataclass
class StatementModule(Statement):
    name: str
    type: Optional[ModuleExpression]
    value: ModuleExpression
