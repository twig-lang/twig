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

  lexer.expect(['id', 'str'])

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
def p_expression(lexer):
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

# 'return' expression ';'
# 'if' expression 'then' statement [ 'else' statement ]
# 'while' expression 'do' statement
# 'loop' statement
# expression
def p_stmt(lexer):
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
    body = p_expr(lexer)
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

# 'function' . name [ argument list ] [ ':' type ] <function body>
def p_function(lexer):
  name = lexer.expect('id')[1]

  arg_list = None
  if lexer.match('('):
    arg_list = p_argument_list(lexer)

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
