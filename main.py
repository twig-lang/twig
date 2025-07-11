#!/usr/bin/env python3

from lexer import lexer
from parse import parse

from sys import argv
from pprint import pp

text = open(argv[1]).read()
lx = lexer(text)
ast = parse(lx)

pp(ast)
