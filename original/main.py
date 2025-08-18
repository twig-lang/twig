from common.sourcemap import add_file
from common.message import report_all

from text.lexer import lexer, Error as LexerError
from text.parse import parse

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

    if not report_all():
        exit(f"Compilation failed.")
