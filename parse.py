def match(lexer, tag):
  if type(tag) is list:
    if match(lexer, tag):
      return True

  return False

def p_toplevel(lexer):
  return next(lexer)

def parse(lexer):
  file = []

  try:
    while True:
      top = p_toplevel(lexer)
      file.append(top)
  except StopIteration:
    pass

  return file
