from frontend.lexer import lexer
from frontend.parse import parse

from sys import argv
from pprint import pp


def main():
    text = open(argv[1]).read()
    lx = lexer(text)
    ast = parse(lx)

    pp(ast)
