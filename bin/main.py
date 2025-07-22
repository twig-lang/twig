from frontend.lexer import lexer, Error as LexerError
from frontend.parse import parse
from frontend.sourcemap import add_file
from frontend.message import MESSAGES

from sys import argv, exit
from pprint import pp


def main():
    path = argv[1]
    text = add_file(path)

    try:
        lx = lexer(path, text)
        ast = parse(lx)
        pp(ast)
    except LexerError:
        pass
    finally:
        pass

    msgs = len(MESSAGES)

    while len(MESSAGES) > 0:
        MESSAGES.popleft().report()

    if msgs > 0:
        exit(f"{msgs} messages printed.")
