def _is_id_head(c):
  return c.isalpha() or c == '_'

def _is_id_tail(c):
  return c.isalnum() or c == '_'

KEYWORDS = {
  'function',
  'begin',
  'end',
  'loop',
  'while',
  'do',
  'if',
  'then',
  'else',
  'yield',
  'break',
  'continue',
  'return',
  'mut',
  'subscript',
  'defer',
  'import',
  'with',
  'module',
  'type',
  'newtype',
  'record',
  'union',
  'variant',
  'enum',
  'match',
  'case',
  'let',
  'in',
  'var',
  'cast',
}

# 1-char long symbols.
# {} are reserved for comments.
SYM1 = {
  '+',
  '-',
  '/',
  '*',
  '%',
  ';',
  '.',
  ',',
  ':',
  '^',
  '~',
  '=',
  '(',
  ')',
  '[',
  ']',
  '#',
}

# 2-char long symbols.
SYM2 = {
  '>': ['>', '='],
  '<': ['<', '='],
  '!': ['='],
  '&': ['&'],
  '|': ['|'],
}

def lexer(text):
  text = text + '  '
  head = iter(text)

  try:
    h = next(head)

    while True:
      word = ''

      if h == '{':
        nesting = 1
        h = next(head)

        while nesting > 0:
          if h == '{':
            nesting += 1

          if h == '}':
            nesting -= 1

          h = next(head)

        continue

      if h.isspace():
        while h.isspace():
          h = next(head)

        continue

      if h.isdigit():
        while h.isdigit():
          word += h
          h = next(head)

        yield ('int', int(word))
        continue

      if _is_id_head(h):
        while _is_id_tail(h):
          word += h
          h = next(head)

        if word in KEYWORDS:
          yield (word,)
        else:
          yield ('id', word)
        continue

      if h == "'":
        h = next(head)
        word = h
        h = next(head)
        assert(h=="'")
        yield('chr', word)
        continue

      if h == '"':
        h = next(head)

        while h != '"':
          word += h
          h = next(head)

        assert(h == '"')
        h = next(head)
        yield('str',word)
        continue

      if h in SYM1:
        yield (h,)
        h = next(head)
        continue

      if h in SYM2:
        word = h
        h = next(head)

        if h in SYM2[word]:
         word += h

        yield (word,)
        h = next(head)
        continue

      h = next(head)
      yield ('unknown', h)
  except StopIteration:
    pass
