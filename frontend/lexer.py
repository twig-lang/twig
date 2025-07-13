from frontend.iter import peekable, Peek


def _is_id_head(c):
    return c.isalpha() or c == "_"


def _is_id_tail(c):
    return c.isalnum() or c == "_"


KEYWORDS = {
    "function",
    "begin",
    "end",
    "loop",
    "while",
    "do",
    "if",
    "then",
    "else",
    "yield",
    "break",
    "continue",
    "return",
    "mut",
    "subscript",
    "defer",
    "import",
    "with",
    "module",
    "type",
    "newtype",
    "record",
    "union",
    "variant",
    "enum",
    "match",
    "case",
    "let",
    "in",
    "cast",
    "set",
}

# 1-char long symbols.
# {} are reserved for comments.
SYM1 = {
    "+",
    "-",
    "/",
    "*",
    "%",
    ";",
    ".",
    ",",
    ":",
    "^",
    "(",
    ")",
    "[",
    "]",
    "#",
}

# 2-char long symbols.
SYM2 = {
    ">": [">"],
    "<": ["<"],
    "!": ["="],
    "&": ["&"],
    "|": ["|"],
    "=": ["<", ">"],
}


def tokenize(text):
    text = text + " "
    head = peekable(iter(text))

    try:
        while True:
            point = head.get_point()

            if head.peek() == "{":
                head.next()
                nesting = 1

                while nesting > 0:
                    if head.peek() == "{":
                        nesting += 1

                    if head.peek() == "}":
                        nesting -= 1

                    head.next()

                continue

            if head.peek().isspace():
                while head.peek().isspace():
                    head.next()

                continue

            if head.peek().isdigit():
                number = ""

                while head.peek().isdigit():
                    number += head.next()

                yield ("int", point, int(number))
                continue

            if _is_id_head(head.peek()):
                ident = ""
                ident += head.next()

                while _is_id_tail(head.peek()):
                    ident += head.next()

                if ident in KEYWORDS:
                    yield (ident, point)
                else:
                    yield ("id", point, ident)

                continue

            if head.peek() == "'":
                head.next()

                chr = head.next()

                assert head.next() == "'"
                yield ("chr", point, chr)
                continue

            if head.peek() == '"':
                str = ""
                head.next()

                while head.peek() != '"':
                    str += head.next()

                assert head.next() == '"'
                yield ("str", point, str)
                continue

            if head.peek() in SYM1:
                yield (head.next(), point)
                continue

            if head.peek() in SYM2:
                sym = head.next()

                if head.peek() in SYM2[sym]:
                    sym += head.next()

                yield (sym, point)
                continue

            yield ("unknown", point, head.next())
    except StopIteration:
        pass


class Lexer:
    def __init__(self, text):
        self.head = peekable(tokenize(text))

    def next(self):
        return self.__next__()

    def peek(self):
        return self.head.peek()

    def at(self, tag):
        if type(tag) is list:
            for t in tag:
                if self.at(t):
                    return True

            return False

        m = self.peek()[0] == tag

        return m

    def match(self, tag):
        m = self.at(tag)

        if m:
            self.next()

        return m

    def expect(self, tag):
        got = self.peek()

        if not self.match(tag):
            raise Exception(f"at {got[1]} expected `{str(tag)}`, got `{got[0]}`")

        return got

    def __next__(self):
        return self.head.next()

    def __iter__(self):
        return self


def lexer(text):
    return Lexer(text)
