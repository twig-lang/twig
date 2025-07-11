from iter import peekable

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
  text = text + ' '
  head = peekable(iter(text))

  try:
    while True:
      if head.peek() == '{':
        head.next()
        nesting = 1

        while nesting > 0:
          if head.peek() == '{':
            nesting += 1

          if head.peek() == '}':
            nesting -= 1

          head.next()

        continue

      if head.peek().isspace():
        while head.peek().isspace():
          head.next()

        continue

      if head.peek().isdigit():
        number = ''

        while head.peek().isdigit():
          number += head.next()

        yield ('int', int(number))
        continue

      if _is_id_head(head.peek()):
        ident = ''
        ident += head.next()

        while _is_id_tail(head.peek()):
          ident += head.next()

        if ident in KEYWORDS:
          yield (ident,)
        else:
          yield ('id', ident)

        continue

      if head.peek() == "'":
        head.next()

        chr = head.next()

        assert(head.next() == "'")
        yield('chr', chr)
        continue

      if head.peek() == '"':
        str = ''
        head.next()

        while head.peek() != '"':
          str += head.next()

        assert(head.next() == '"')
        yield('str', str)
        continue

      if head.peek() in SYM1:
        yield (head.next(),)
        continue

      if head.peek() in SYM2:
        sym = head.next()

        if head.peek() in SYM2[word]:
         sym += head.next()

        yield (sym,)
        continue

      yield ('unknown', head.next())
  except StopIteration:
    pass
