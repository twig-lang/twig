#!/usr/bin/env python3

# Token: ( tag, data... )

from lexer import lexer
from sys import argv

text = open(argv[1]).read()
lx = lexer(text)

for token in lx:
  print(token)
