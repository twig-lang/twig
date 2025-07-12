def p_literal(lexer):
  if lexer.at('id'):
    id = lexer.expect('id')

    return {
      'tag': 'variable',
      'name': id[1]
    }

  if lexer.at('str'):
    str = lexer.expect('str')

    return {
      'tag': 'string',
      'value': str[1]
    }

  if lexer.at('int'):
    int = lexer.expect('int')

    return {
      'tag': 'int',
      'value': int[1]
    }

  lexer.expect(['id', 'str', 'int'])

# literal
# '(' expression ')'
def p_primary(lexer):
  return p_literal(lexer)

# expressino
def p_argument(lexer):
  return p_expression(lexer)

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

  return {
    'tag': 'arguments',
    'value': args
  }

# inner expression '(' arguments ')'
# inner expression '[' arguments ']'
def p_factor(lexer):
  inner = p_primary(lexer)

  while True:
    if lexer.match('('):
      args = p_arguments(lexer, ')')
      inner = {
        'tag': 'expr_fncall',
        'callee': inner,
        'arguments': args
      }

    elif lexer.match('['):
      args = p_arguments(lexer, ']')
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

def p_bin_rhs(lexer, prec, lhs):
  while True:
    op = lexer.peek()[0]

    if op not in PRECEDENCES:
      return lhs

    oprec = PRECEDENCES[op]

    if oprec < prec:
      return lhs

    lexer.next()

    rhs = p_factor(lexer)

    nextop = lexer.peek()[0]

    if nextop in PRECEDENCES:
      nextprec = PRECEDENCES[nextop]

      if nextprec > prec:
        rhs = p_bin_rhs(prec+1,rhs)

    lhs = {
      'tag': 'binary_expr',
      'lhs': lhs,
      'rhs': rhs,
    }

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

  return {
    'tag': 'stmt_begin',
    'children': children
  }

# 'let' . [ mode ] name [ ':' type ] '=' expression ';'
def p_let(lexer):
  mode = p_mode(lexer)

  name = lexer.expect('id')[1]

  ty = None
  if lexer.match(':'):
    ty = p_type(lexer)

  lexer.expect('=')

  value = p_expression(lexer)

  lexer.expect(';')

  return {
    'tag': 'stmt_let',
    'name': name,
    'mode': mode,
    'type': ty,
    'value': value,
  }

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

  expr = p_expression(lexer)
  lexer.expect(';')

  return {
    'tag': 'stmt_expr',
    'value': expr,
  }

# '=' expr ';'
# ';'
# statement
def p_function_body(lexer):
  if lexer.match(';'):
    return {
      'tag': 'body_declaration',
    }

  if lexer.match('='):
    body = p_expression(lexer)
    lexer.expect(';')

    return {
      'tag': 'body_expr',
      'body': body,
    }

  body = p_stmt(lexer)
  return {
    'tag': 'body_stmt',
    'body': body,
  }

# uhhhh
# name
def p_type(lexer):
  name = lexer.expect('id')[1]
  return {
    'tag': 'type_named',
    'name': name
  }

# [ '&' ] [ 'mut' ]
def p_mode(lexer):
  mutable = False
  reference = False

  if lexer.match('&'):
    reference = True

  if lexer.match('mut'):
    mutable = True

  return {
    'tag': 'mode',
    'reference': reference,
    'mutable': mutable,
  }

# [ mode ] name ':' type
def p_param(lexer):
  mode = p_mode(lexer)

  name = lexer.expect('id')[1]

  lexer.expect(':')

  ty = p_type(lexer)

  return {
    'tag': 'argument',
    'mode': mode,
    'name': name,
    'type': ty,
  }

# '(' . [ argument { ',' argument } ] ')'
def p_param_list(lexer):
  args = []

  while True:
    arg = p_param(lexer)
    args.append(arg)

    if lexer.match(')'):
      break

    lexer.expect(',')

  return args

# 'function' . name [ argument list ] [ ':' type ] <function body>
def p_function(lexer):
  name = lexer.expect('id')[1]

  arg_list = None
  if lexer.match('('):
    arg_list = p_param_list(lexer)

  arg_list = arg_list or []

  ty = None
  if lexer.match(':'):
    ty = p_type(lexer)

  body = p_function_body(lexer)

  return {
    'tag': 'function_definition',
    'name': name,
    'arguments': arg_list,
    'returns': ty,
    'body': body,
  }

# identifier [ '.' with-path ]
# '(' [ with-path { ',' with-path } ] ')'
def p_with_path(lexer):
  if lexer.at('id'):
    name = lexer.expect('id')[1]
    path = {
      'tag': 'path_name',
      'name': name,
    }

    while lexer.match('.'):
      sub = p_with_path(lexer)

      path = {
        'tag': 'path_sub',
        'parent': path,
        'child': sub
      }

    return path

  lexer.expect('(')

  children = []

  if lexer.at('id'):
    child = p_with_path(lexer)
    children.append(child)

  while lexer.match(','):
    if lexer.at('id'):
      child = p_with_path(lexer)
      children.append(child)
    else:
      lexer.expect('id')

  lexer.expect(')')

  return {
    'tag': 'with_members',
    'members': children
  }

# 'with' with-path ';'
# 'with' 'import' with-path ';'
def p_with(lexer):
  imports = False

  if lexer.match('import'):
    imports = True

  path = p_with_path(lexer)

  lexer.expect(';')

  return {
    'tag': 'with',
    'imports': imports,
    'path': path,
  }

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
