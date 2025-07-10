def _is_id_head(c):
  return c.isalpha() or c == '_'

def _is_id_tail(c):
  return c.isalnum() or c == '_'

KEYWORDS = {
  'function',
  'return',
}

def lexer(text):
  head = iter(text)

  try:
    h = next(head)

    while True:
      word = ''

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

      if h == ';':
        yield (';',)

      h = next(head)
      yield ('unknown', h)
  except StopIteration:
    pass
