#!/usr/bin/env python3

# Token: ( tag, data... )

from lexer import lexer

text = 'function bool example return true { { && } false };'
lx = lexer(text)

for token in lx:
  print(token)
