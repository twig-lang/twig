#!/usr/bin/env python3

# Token: ( tag, data... )

from lexer import lexer
from parse import parse
from sys import argv

text = open(argv[1]).read()
lx = lexer(text)
ast = parse(lx)

print(ast)
