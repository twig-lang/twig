from frontend.lexer import lexer
from frontend.parse import parse
from frontend.sourcemap import add_file
from frontend.message import MESSAGES

from sys import argv
from pprint import pp


def main():
    path = argv[1]
    text = add_file(path)
    lx = lexer(path, text)
    ast = parse(lx)

    while len(MESSAGES) > 0:
        MESSAGES.popleft().report()

    pp(ast)
