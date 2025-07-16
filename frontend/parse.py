from typing import Optional

from frontend.lexer import Error as LexerError
from frontend.lexer import Lexer
from frontend.sourcemap import KNOWN_FILES
from frontend import syntax, message as msg
from frontend.message import Message
from frontend.token import *


# function jumps to a known token, and then it may consume it or not.
def recovers_on(tokens, consumes=False):
    def deco(function):
        def wrapper(lexer, *args, **kwargs):
            try:
                return function(lexer, *args, **kwargs)
            except LexerError:
                while not lexer.at(tokens):
                    lexer.next()

                if consumes:
                    lexer.next()

                return None

        return wrapper

    return deco


def p_name(lexer):
    name = lexer.expect(Tag.Identifier).data
    return syntax.Name(name)


def p_lit_str(lexer):
    value = lexer.expect(Tag.String).data
    return syntax.StringLiteral(value)


def p_lit_int(lexer):
    value = lexer.expect(Tag.Int).data
    return syntax.IntegerLiteral(value)


def p_literal(lexer):
    if lexer.at(Tag.Identifier):
        return p_path(lexer)

    if lexer.at(Tag.String):
        return p_lit_str(lexer)

    if lexer.at(Tag.Int):
        return p_lit_int(lexer)

    lexer.expect([Tag.Identifier, Tag.String, Tag.Int])


def p_primary(lexer):
    if lexer.match(Tag.PLParen):
        inner = p_expression(lexer)
        lexer.expect(Tag.PRParen)

        return inner

    return p_literal(lexer)


def p_argument(lexer):
    mode = p_mode(lexer)
    key = None

    value = p_expression(lexer)

    if lexer.match(Tag.PColon):
        assert type(value) is syntax.Name
        key = value
        value = p_expression(lexer)

    return syntax.Argument(mode, key, value)


def p_arguments(lexer, end):
    arguments = []

    while True:
        arg = p_argument(lexer)
        arguments.append(arg)

        if lexer.at(end):
            break

        lexer.expect(Tag.PComma)

    lexer.expect(end)

    return syntax.ArgumentList(arguments)


def p_factor(lexer):
    inner = p_primary(lexer)

    while True:
        if lexer.match(Tag.PLParen):
            arguments = p_arguments(lexer, Tag.PRParen)
            inner = syntax.ExpressionFunctionCall(callee=inner, arguments=arguments)

        elif lexer.match(Tag.PLBracket):
            arguments = p_arguments(lexer, Tag.PRBracket)
            inner = syntax.ExpressionSubscriptCall(callee=inner, arguments=arguments)

        else:
            break

    return inner


PRECEDENCES = {
    Tag.OpDiv: 9,
    Tag.OpMul: 9,
    Tag.OpMod: 9,
    Tag.OpAdd: 8,
    Tag.OpSub: 8,
    Tag.OpShr: 7,
    Tag.OpShl: 7,
    Tag.OpAnd: 6,
    Tag.OpXor: 5,
    Tag.OpOr: 4,
    Tag.OpLt: 3,
    Tag.OpGt: 3,
    Tag.OpEq: 3,
    Tag.OpNotEq: 3,
    Tag.OpEqLt: 3,
    Tag.OpEqGt: 3,
    Tag.OpAndthen: 2,
    Tag.OpOrelse: 1,
}


def precedence(operator):
    prec = PRECEDENCES[operator]
    return prec * 100


def p_operator(lexer) -> Optional[tuple[syntax.Operator, int]]:
    operator = lexer.peek().tag

    if operator not in PRECEDENCES:
        return None

    precedence = PRECEDENCES[operator]
    operator = syntax.Operator(operator)

    return (operator, precedence)


def p_bin_rhs(lexer, prec, lhs):
    while True:
        op = p_operator(lexer)

        if not op:
            return lhs

        operator, oprec = op

        if oprec < prec:
            return lhs

        lexer.next()

        rhs = p_factor(lexer)

        nextop = p_operator(lexer)

        if nextop:
            nextop, nextprec = nextop

            if nextprec > prec:
                rhs = p_bin_rhs(prec + 1, rhs)

        lhs = syntax.ExpressionBinary(operator, lhs, rhs)


def p_expression(lexer):
    lhs = p_factor(lexer)

    return p_bin_rhs(lexer, 0, lhs)


@recovers_on(Tag.KwEnd, consumes=True)
def p_begin(lexer):
    children = []

    while True:
        if lexer.match(Tag.KwEnd):
            break

        child = p_stmt(lexer)
        children.append(child)

    return syntax.StatementBegin(children)


def p_let_binding(lexer):
    mode = p_mode(lexer)

    name = p_name(lexer)

    type = None
    if lexer.match(Tag.PColon):
        type = p_type(lexer)

    lexer.expect(Tag.OpEq)

    value = p_expression(lexer)

    lexer.expect(Tag.PSemicolon)

    return syntax.LetBinding(mode, name, type, value)


def p_let(lexer):
    if lexer.match(Tag.KwBegin):
        bindings = []

        while not lexer.match(Tag.KwEnd):
            binding = p_let_binding(lexer)
            bindings.append(binding)

        return syntax.StatementLet(bindings)

    binding = p_let_binding(lexer)

    return syntax.StatementLet(bindings=[binding])


def p_while(lexer):
    condition = p_expression(lexer)

    lexer.expect(Tag.KwDo)

    body = p_stmt(lexer)

    return syntax.StatementWhile(condition, body)


def p_set_binding(lexer):
    lvalue = p_name(lexer)

    operator = None

    if not lexer.at(Tag.OpEq):
        operator = p_operator(lexer)

        if operator:
            lexer.next()
            operator, _ = operator

    lexer.expect(Tag.OpEq)

    rvalue = p_expression(lexer)

    lexer.expect(Tag.PSemicolon)

    return syntax.SetBinding(lvalue, operator, rvalue)


def p_set(lexer):
    if lexer.match(Tag.KwBegin):
        bindings = []

        while not lexer.match(Tag.KwEnd):
            binding = p_set_binding(lexer)
            bindings.append(binding)

        return syntax.StatementSet(bindings)

    binding = p_set_binding(lexer)

    return syntax.StatementSet(bindings=[binding])


@recovers_on(Tag.PSemicolon, consumes=True)
def p_return(lexer):
    value = None

    if not lexer.at(Tag.PSemicolon):
        value = p_expression(lexer)

    lexer.expect(Tag.PSemicolon)

    return syntax.StatementReturn(value)


@recovers_on(Tag.PSemicolon, consumes=True)
def p_yield(lexer):
    mode = p_mode(lexer)
    value = p_expression(lexer)
    lexer.expect(Tag.PSemicolon)

    return syntax.StatementYield(mode, value)


def p_if(lexer):
    condition = p_expression(lexer)

    lexer.expect(Tag.KwThen)

    taken = p_stmt(lexer)
    not_taken = None

    if lexer.match(Tag.KwElse):
        not_taken = p_stmt(lexer)

    return syntax.StatementIf(condition, taken, not_taken)


def p_stmt(lexer):
    if lexer.match(Tag.KwBegin):
        return p_begin(lexer)

    if lexer.match(Tag.KwLet):
        return p_let(lexer)

    if lexer.match(Tag.KwWhile):
        return p_while(lexer)

    if lexer.match(Tag.KwSet):
        return p_set(lexer)

    if lexer.match(Tag.KwReturn):
        return p_return(lexer)

    if lexer.match(Tag.KwIf):
        return p_if(lexer)

    if lexer.match(Tag.KwYield):
        return p_yield(lexer)

    expression = p_expression(lexer)
    lexer.expect(Tag.PSemicolon)

    return syntax.StatementExpression(expression)


def p_function_body(lexer):
    if lexer.match(Tag.OpEq):
        body = p_expression(lexer)
        lexer.expect(Tag.PSemicolon)

        return syntax.FunctionBodyExpression(body)

    body = p_stmt(lexer)

    return syntax.FunctionBodyStatement(body)


@recovers_on([Tag.PComma, Tag.PSemicolon])
def p_member(lexer):
    name = p_name(lexer)
    lexer.expect(Tag.PColon)
    type = p_type(lexer)
    return syntax.Member(name, type)


@recovers_on(Tag.PSemicolon)
def p_record(lexer):
    members = []

    if lexer.at(Tag.PSemicolon):
        return syntax.TypeRecord([])

    mem = p_member(lexer)
    members.append(mem)

    while lexer.match(Tag.PComma):
        mem = p_member(lexer)
        members.append(mem)

    return syntax.TypeRecord(members)


@recovers_on(Tag.PSemicolon)
def p_union(lexer):
    members = []

    if lexer.at(Tag.PSemicolon):
        return syntax.TypeRecord([])

    mem = p_member(lexer)
    members.append(mem)

    while lexer.match(Tag.PComma):
        mem = p_member(lexer)
        members.append(mem)

    return syntax.TypeUnion(members)


@recovers_on(Tag.PSemicolon)
def p_enum(lexer):
    names = []

    if lexer.at(Tag.PSemicolon):
        return syntax.TypeEnum([])

    n = p_name(lexer)
    names.append(n)

    while lexer.match(Tag.PComma):
        n = p_name(lexer)
        names.append(n)

    return syntax.TypeEnum(names)


def p_tuple(lexer):
    members = []

    if not lexer.at(Tag.PRParen):
        ty = p_type(lexer)
        members.append(ty)
    else:
        members.append(syntax.TypeUnit())

    while lexer.match(Tag.PComma):
        ty = p_type(lexer)
        members.append(ty)

    lexer.expect(Tag.PRParen)

    if len(members) == 1:
        return members[0]
    else:
        return syntax.TypeTuple(members)


@recovers_on([Tag.PComma, Tag.PSemicolon])
def p_varcase(lexer):
    name = p_name(lexer)
    arguments = []

    if lexer.match(Tag.PLParen):
        if not lexer.at(Tag.PRParen):
            arg = p_type(lexer)
            arguments.append(arg)

            while lexer.match(Tag.PComma):
                arg = p_type(lexer)
                arguments.append(arg)

        lexer.expect(Tag.PRParen)

    return syntax.Variant(name, arguments)


@recovers_on(Tag.PSemicolon)
def p_variant(lexer):
    members = []

    if lexer.at(Tag.PSemicolon):
        return syntax.TypeVariant(members)

    mem = p_varcase(lexer)
    members.append(mem)

    while lexer.match(Tag.PComma):
        mem = p_varcase(lexer)
        members.append(mem)

    return syntax.TypeVariant(members)


def p_type(lexer, on_def=False):
    if lexer.match(Tag.PLParen):
        return p_tuple(lexer)

    if on_def:
        if lexer.match(Tag.KwRecord):
            return p_record(lexer)

        if lexer.match(Tag.KwUnion):
            return p_union(lexer)

        if lexer.match(Tag.KwEnum):
            return p_enum(lexer)

        if lexer.match(Tag.KwVariant):
            return p_variant(lexer)

    name = p_path(lexer)

    return syntax.TypeNamed(name)


def p_mode(lexer):
    mode = syntax.Mode(0)

    if lexer.match(Tag.OpAnd):
        mode |= syntax.Mode.REFERENCE

    if lexer.match(Tag.KwMut):
        mode |= syntax.Mode.MUTABLE

    return mode


def p_param(lexer):
    mode = p_mode(lexer)

    name = p_name(lexer)
    key = None

    if lexer.at(Tag.Identifier):
        key = p_name(lexer)

    lexer.expect(Tag.PColon)

    type = p_type(lexer)

    default = None
    if key is not None:
        lexer.expect(Tag.OpEq)
        default = p_expression(lexer)

    return syntax.Parameter(mode, name, key, type, default)


def p_param_list(lexer, begin, end):
    params = []

    if lexer.match(begin):
        while True:
            par = p_param(lexer)
            params.append(par)

            if lexer.match(end):
                break

            lexer.expect(Tag.PComma)

    return syntax.ParameterList(parameters=params)


def p_function(lexer):
    name = p_name(lexer)

    parameters = p_param_list(lexer, Tag.PLParen, Tag.PRParen)

    return_type = None

    if lexer.match(Tag.PColon):
        return_type = p_type(lexer)

    body = p_function_body(lexer)

    return syntax.FunctionDefinition(name, parameters, return_type, body)


def p_path_arg(lexer):
    key = None
    name = p_name(lexer)
    value = None

    if lexer.match(Tag.PColon):
        key = name
        value = p_path(lexer)
    else:
        name = syntax.PathNamed(name)
        value = p_path(lexer, lhs=name)

    return syntax.ModuleArgument(key, value)


def p_path_args(lexer):
    args = []

    while True:
        if lexer.at(Tag.PRParen):
            break

        arg = p_path_arg(lexer)
        args.append(arg)

        if not lexer.at(Tag.PRParen):
            lexer.expect(Tag.PComma)

    lexer.expect(Tag.PRParen)

    return args


def p_path(lexer, as_with=False, lhs=None):
    path = lhs

    if not path:
        name = p_name(lexer)
        path = syntax.PathNamed(name)

    while lexer.match(Tag.PDot):
        child = None

        if as_with and lexer.at(Tag.LParen):
            child = p_with_path(lexer)
        else:
            child = p_path(lexer)

        path = syntax.PathSub(parent=path, child=child)

    if lexer.match(Tag.PLParen):
        arguments = p_path_args(lexer)
        path = syntax.PathCall(callee=path, arguments=arguments)
        return p_path(lexer, as_with=as_with, lhs=path)

    return path


def p_with_path(lexer):
    if lexer.at(Tag.Identifier):
        return p_path(lexer, as_with=True)

    lexer.expect(Tag.PLParen)

    members = []

    if lexer.at(Tag.Identifier):
        member = p_with_path(lexer)
        members.append(member)

    while lexer.match(Tag.PComma):
        if lexer.at(Tag.Identifier):
            member = p_with_path(lexer)
            members.append(member)
        else:
            lexer.expect(Tag.Identifier)

    lexer.expect(Tag.PRParen)

    return syntax.WithPathMembers(members)


@recovers_on(Tag.PSemicolon, consumes=True)
def p_with(lexer):
    imports = False

    if lexer.match(Tag.KwImport):
        imports = True

    path = p_with_path(lexer)

    lexer.expect(Tag.PSemicolon)

    return syntax.StatementWith(imports, path)


@recovers_on(Tag.PSemicolon, consumes=True)
def p_import(lexer):
    path = p_path(lexer)

    lexer.expect(Tag.PSemicolon)

    return syntax.Import(path)


def p_subscript(lexer):
    mode = p_mode(lexer)

    name = p_name(lexer)

    parameters = p_param_list(lexer, Tag.PLBracket, Tag.PRBracket)

    return_type = None

    if lexer.match(Tag.PColon):
        return_type = p_type(lexer)

    body = p_function_body(lexer)

    return syntax.SubscriptDefinition(name, mode, parameters, return_type, body)


@recovers_on(Tag.PSemicolon, consumes=True)
def p_typedef(lexer):
    name = p_name(lexer)

    lexer.expect(Tag.OpEq)

    type = p_type(lexer, on_def=True)

    lexer.expect(Tag.PSemicolon)

    return syntax.TypeDefinition(name, type)


TOPLEVEL_TOKENS = [
    Tag.KwFunction,
    Tag.KwSubscript,
    Tag.KwWith,
    Tag.KwImport,
    Tag.KwType,
]


@recovers_on(TOPLEVEL_TOKENS)
def p_toplevel(lexer):
    if lexer.match(Tag.KwFunction):
        return p_function(lexer)

    if lexer.match(Tag.KwSubscript):
        return p_subscript(lexer)

    if lexer.match(Tag.KwWith):
        return p_with(lexer)

    if lexer.match(Tag.KwImport):
        return p_import(lexer)

    if lexer.match(Tag.KwType):
        return p_typedef(lexer)

    return lexer.next()


def parse(lexer):
    file = []

    try:
        while True:
            top = p_toplevel(lexer)
            file.append(top)
    except StopIteration:
        pass
    finally:
        pass

    return file
