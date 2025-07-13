from typing import Optional

from lexer import Lexer
import syntax

def p_variable(lexer):
  name = lexer.expect('id')[2]
  return syntax.Variable(name)

def p_lit_str(lexer):
    str = lexer.expect('str')
    return syntax.StringLiteral(value = str[2])

def p_lit_int(lexer):
    int = lexer.expect('int')
    return syntax.IntegerLiteral(value = int[2])

def p_literal(lexer):
  if lexer.at('id'):
    return p_variable(lexer)

  if lexer.at('str'):
    return p_lit_str(lexer)

  if lexer.at('int'):
    return p_lit_int(lexer)

  lexer.expect(['id', 'str', 'int'])

# literal
# '(' expression ')'
def p_primary(lexer):
  if lexer.match('('):
    inner = p_expression(lexer)
    lexer.expect(')')

    return inner

  return p_literal(lexer)

# expression
def p_argument(lexer):
  mode = p_mode(lexer)
  key = None

  value = p_expression(lexer)

  if lexer.match(':'):
    assert(type(value) is syntax.Variable)
    key = value
    value = p_expression(lexer)

  return syntax.Argument(mode, key, value)

# argument
# argument ',' arguments
def p_arguments(lexer, end):
  args = []

  while True:
    arg = p_argument(lexer)
    args.append(arg)

    if lexer.at(end):
      break

    lexer.expect(',')

  lexer.expect(end)

  return syntax.ArgumentList(arguments = args)

# inner expression '(' arguments ')'
# inner expression '[' arguments ']'
def p_factor(lexer):
  inner = p_primary(lexer)

  while True:
    if lexer.match('('):
      arguments = p_arguments(lexer, ')')
      inner = syntax.ExpressionFunctionCall(
        callee = inner,
        arguments = arguments
      )

    elif lexer.match('['):
      arguments = p_arguments(lexer, ']')
      inner = {
        'tag': 'expr_subcall',
        'callee': inner,
        'arguments': args
      }

    else:
      break

  return inner

PRECEDENCES = {
  '/': 9,
  '*': 9,
  '%': 9,

  '+': 8,
  '-': 8,

  '>>': 7,
  '<<': 7,

  '&': 6,

  '^': 5,

  '|': 4,

  '<': 3,
  '>': 3,
  '=': 3,
  '!=': 3,
  '=<': 3,
  '=>': 3,

  '&&': 2,
  '||': 1,
}

def precedence(operator):
  prec = PRECEDENCES[operator]
  return prec * 100

def p_operator(lexer) -> Optional[tuple[syntax.Operator, int]]:
  operator = lexer.peek()[0]

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
        rhs = p_bin_rhs(prec+1,rhs)

    lhs = syntax.ExpressionBinary(operator, lhs, rhs)

def p_expression(lexer):
  lhs = p_factor(lexer)

  return p_bin_rhs(lexer, 0, lhs)

def p_begin(lexer):
  children = []

  while True:
    if lexer.match('end'):
      break

    child = p_stmt(lexer)
    children.append(child)

  return syntax.StatementBegin(children)

def p_let_binding(lexer):
  mode = p_mode(lexer)

  name = p_variable(lexer)

  type = None
  if lexer.match(':'):
    type = p_type(lexer)

  lexer.expect('=')

  value = p_expression(lexer)

  lexer.expect(';')

  return syntax.LetBinding(
    mode,
    name,
    type,
    value
  )

# 'let' . [ mode ] name [ ':' type ] '=' expression ';'
def p_let(lexer):
  if lexer.match('begin'):
    bindings = []

    while not lexer.match('end'):
      binding = p_let_binding(lexer)
      bindings.append(binding)

    return syntax.StatementLet(bindings)


  binding = p_let_binding(lexer)

  return syntax.StatementLet(
    bindings = [binding]
  )

def p_while(lexer):
  condition = p_expression(lexer)

  lexer.expect('do')

  body = p_stmt(lexer)

  return syntax.StatementWhile(condition, body)

def p_set_binding(lexer):
  lvalue = p_variable(lexer)

  operator = None

  if not lexer.at('='):
    operator = p_operator(lexer)

    if operator:
      lexer.next()
      operator,_ = operator

  lexer.expect('=')

  rvalue = p_expression(lexer)

  lexer.expect(';')

  return syntax.SetBinding(lvalue, operator, rvalue)

# 'set' . name [ operator ] '=' expression ';'
def p_set(lexer):
  if lexer.match('begin'):
    bindings = []

    while not lexer.match('end'):
      binding = p_set_binding(lexer)
      bindings.append(binding)

    return syntax.StatementSet(bindings)

  binding = p_set_binding(lexer)

  return syntax.StatementSet(
    bindings = [binding]
  )

def p_return(lexer):
  value = None

  if not lexer.at(';'):
    value = p_expression(lexer)

  lexer.expect(';')

  return syntax.StatementReturn(value)

def p_if(lexer):
  condition = p_expression(lexer)

  lexer.expect('then')

  taken = p_stmt(lexer)
  not_taken = None

  if lexer.match('else'):
    not_taken = p_stmt(lexer)

  return syntax.StatementIf(condition, taken, not_taken)

# 'return' expression ';'
# 'if' expression 'then' statement [ 'else' statement ]
# 'while' expression 'do' statement
# 'loop' statement
# expression
def p_stmt(lexer):
  if lexer.match('begin'):
    return p_begin(lexer)

  if lexer.match('let'):
    return p_let(lexer)

  if lexer.match('while'):
    return p_while(lexer)

  if lexer.match('set'):
    return p_set(lexer)

  if lexer.match('return'):
    return p_return(lexer)

  if lexer.match('if'):
    return p_if(lexer)

  expression = p_expression(lexer)
  lexer.expect(';')

  return syntax.StatementExpression(expression)

# '=' expr ';'
# statement
def p_function_body(lexer):
  if lexer.match('='):
    body = p_expression(lexer)
    lexer.expect(';')

    return syntax.FunctionBodyExpression(body)

  body = p_stmt(lexer)

  return syntax.FunctionBodyStatement(body)

# uhhhh
# name
def p_type(lexer):
  name = p_variable(lexer)

  return syntax.TypeNamed(name)

# [ '&' ] [ 'mut' ]
def p_mode(lexer):
  mode = syntax.Mode(0)

  if lexer.match('&'):
    mode |= syntax.Mode.REFERENCE

  if lexer.match('mut'):
    mode |= syntax.Mode.MUTABLE

  return mode

# [ mode ] name ':' type
def p_param(lexer):
  mode = p_mode(lexer)

  name = p_variable(lexer)
  key = None

  if lexer.at('id'):
    key = p_variable(lexer)

  lexer.expect(':')

  type = p_type(lexer)

  default = None
  if key is not None:
    lexer.expect('=')
    default = p_expression(lexer)

  return syntax.Parameter(
    mode,
    name,
    key,
    type,
    default
  )

# '(' . [ argument { ',' argument } ] ')'
def p_param_list(lexer):
  params = []

  if lexer.match('('):
    while True:
      par = p_param(lexer)
      params.append(par)

      if lexer.match(')'):
        break

      lexer.expect(',')

  return syntax.ParameterList(
    parameters = params
  )

# 'function' . name [ argument list ] [ ':' type ] <function body>
def p_function(lexer):
  name = p_variable(lexer)

  parameters = p_param_list(lexer)

  return_type = None

  if lexer.match(':'):
    return_type = p_type(lexer)

  body = p_function_body(lexer)

  return syntax.FunctionDefinition(
    name,
    parameters,
    return_type,
    body
  )

# identifier [ '.' with-path ]
# '(' [ with-path { ',' with-path } ] ')'
def p_with_path(lexer):
  if lexer.at('id'):
    name = p_variable(lexer)

    path = syntax.PathNamed(name)

    while lexer.match('.'):
      child = p_with_path(lexer)

      path = syntax.PathSub(
        parent = path,
        child = child
      )

    return path

  lexer.expect('(')

  members = []

  if lexer.at('id'):
    member = p_with_path(lexer)
    members.append(member)

  while lexer.match(','):
    if lexer.at('id'):
      member = p_with_path(lexer)
      members.append(member)
    else:
      lexer.expect('id')

  lexer.expect(')')

  return syntax.WithPathMembers(members)

# 'with' with-path ';'
# 'with' 'import' with-path ';'
def p_with(lexer):
  imports = False

  if lexer.match('import'):
    imports = True

  path = p_with_path(lexer)

  lexer.expect(';')

  return syntax.StatementWith(
    imports,
    path
  )

def p_toplevel(lexer):
  if lexer.match('function'):
    return p_function(lexer)

  if lexer.match('with'):
    return p_with(lexer)

  return lexer.next()

def parse(lexer):
  file = []

  try:
    while True:
      top = p_toplevel(lexer)
      file.append(top)
  except StopIteration:
    pass

  return file
