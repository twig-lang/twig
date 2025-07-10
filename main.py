#!/usr/bin/env python3

# Token: ( tag, data... )

from lexer import lexer

text = 'function example return 0;'
lx = lexer(text)

for token in lx:
  print(token)
