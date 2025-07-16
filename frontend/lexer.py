from frontend.iter import peekable, Peek
from frontend.message import Message
import frontend.message as msg
from frontend.token import *


def _is_id_head(c):
    return c.isalpha() or c == "_"


def _is_id_tail(c):
    return c.isalnum() or c == "_"


class Error(Exception):
    pass


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

                span = (point, head.get_point())
                yield Token(Tag.Int, span, data=int(number))
                continue

            if _is_id_head(head.peek()):
                ident = ""
                ident += head.next()

                while _is_id_tail(head.peek()):
                    ident += head.next()

                span = (point, head.get_point())

                try:
                    tag = KEYWORDS[ident]
                    yield Token(tag, span)
                except:
                    yield Token(Tag.Identifier, span, data=ident)

                continue

            if head.peek() == "'":
                head.next()

                chr = head.next()

                assert head.next() == "'"

                span = (point, head.get_point())
                yield Token(Tag.Char, span, data=chr)
                continue

            if head.peek() == '"':
                str = ""
                head.next()

                while head.peek() != '"':
                    str += head.next()

                assert head.next() == '"'

                span = (point, head.get_point())
                yield Token(Tag.String, span, str)
                continue

            if head.peek() in SYM_1:
                tag = SYM_1[head.next()]
                span = (point, head.get_point())
                yield Token(tag, span)
                continue

            if head.peek() in SYM_2:
                tag, peekmap = SYM_2[head.next()]

                if head.peek() in peekmap:
                    tag = peekmap[head.next()]

                span = (point, head.get_point())
                yield Token(tag, span)
                continue

            yield ("unknown", point, head.next())
    except StopIteration:
        pass


class Lexer:
    def __init__(self, path, text):
        self.path = path
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

        m = self.peek().tag == tag

        return m

    def match(self, tag):
        m = self.at(tag)

        if m:
            self.next()

        return m

    def expect(self, tag):
        got = self.peek()
        self.next()

        if isinstance(tag, list):
            if got.tag not in tag:
                values = list(map(lambda x: x.value, tag))

                if len(values) > 1:
                    values[-1] = "or " + values[-1]

                tags = ", ".join(values)

                msg.send(
                    Message(
                        message=f"expected one of {tags}, got {got.tag.value}",
                        path=self.path,
                        span=got.span,
                    )
                )

                raise Error()

        elif got.tag != tag:
            msg.send(
                Message(
                    message=f"expected {tag.value}, got {got.tag.value}",
                    path=self.path,
                    span=got.span,
                )
            )

            raise Error()

        return got

    def __next__(self):
        return self.head.next()

    def __iter__(self):
        return self


def lexer(path, text):
    return Lexer(path, text)
