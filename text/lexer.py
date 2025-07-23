from common.iter import peekable, Peek
import common.message as msg
from common.message import Message

from text.token import *


def _is_id_head(c):
    return c.isalpha() or c == "_"


def _is_id_tail(c):
    return c.isalnum() or c == "_"


class Error(Exception):
    pass


def _ishex(c):
    return c in "abcdefABCDEF0123456789"


def l_char(head, path=""):
    c = head.next()
    point = head.get_point()

    if c == "\\":
        c = head.next()
        span = (point, head.get_point())

        if c == "r":
            return ord("\r")
        elif c == "0":
            return 0
        elif c == "n":
            return ord("\n")
        elif c == "t":
            return ord("\t")
        elif c == "x":
            low = head.next()
            hi = head.next()

            assert _ishex(low)
            assert _ishex(hi)

            return bytes.fromhex(low + hi)[0]
        elif c == "u":
            assert head.next() == "{"

            code = ""
            while head.peek() != "}":
                c = head.next()
                if c == "_":
                    continue
                code += c

            assert head.next() == "}"

            return int(code, base=16)

        elif c == '"':
            return ord('"')
        elif c == "'":
            return ord("'")
        elif c == "\\":
            return ord("\\")
        else:
            msg.send(
                Message(message=f"unknown character escape `{c}`", span=span, path=path)
            )

            raise Error()

    return ord(c)


def l_str(head, point, raw=False, as_bytes=False, path=""):
    nhash = 0

    if raw:
        while head.peek() == "#":
            nhash += 1
            head.next()

    assert head.next() == '"'

    data = []

    while True:
        while head.peek() != '"':
            if raw:
                data.append(ord(head.next()))
            else:
                data.append(l_char(head, path=path))

        # head.next() can only be "
        head.next()

        # count hashes, if any
        if raw:
            gothash = 0

            while head.peek() == "#":
                gothash += 1
                head.next()

                if nhash == gothash:
                    break

            # if not enough were found, add them to the string and keep going
            if nhash != gothash:
                data.extend([ord('"')] + [ord("#")] * gothash)
            else:
                break
        else:
            break

    span = (point, head.get_point())

    tag = Tag.Bytestring

    if as_bytes:
        data = bytes(data)
    else:
        tag = Tag.String
        data = "".join(map(chr, data))

    return Token(tag, span, data)


def l_chr(head, point, byte=False, path=""):
    head.next()

    chr = l_char(head, path=path)

    assert head.next() == "'"

    if byte:
        assert chr < 255

    span = (point, head.get_point())

    tag = Tag.Bytechar if byte else Tag.Char
    chr = ord(chr)

    return Token(tag, span, data=chr)


def tokenize(path, text):
    text = text + " "
    head = peekable(iter(text))
    can_eof = False
    point = None

    try:
        while True:
            point = head.get_point()

            can_eof = True

            if head.peek().isspace():
                while head.peek().isspace():
                    head.next()

                continue

            can_eof = False

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

                if head.peek() in '"#':
                    if ident == "r":
                        yield l_str(head, point, raw=True, path=path)
                        continue

                    if ident == "rb":
                        yield l_str(head, point, as_bytes=True, raw=True, path=path)
                        continue

                    if ident == "b":
                        yield l_str(head, point, as_bytes=True, path=path)
                        continue

                if head.peek() == "'":
                    if ident == "b" and head.peek() == "'":
                        yield l_chr(head, point, byte=True, path=path)
                        continue

                span = (point, head.get_point())

                try:
                    tag = KEYWORDS[ident]
                    yield Token(tag, span)
                except:
                    yield Token(Tag.Identifier, span, data=ident)

                continue

            if head.peek() == "'":
                yield l_chr(head, point, path=path)
                continue

            if head.peek() == '"':
                yield l_str(head, point, path=path)
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

            chr = head.next()
            span = (point, head.get_point())

            msg.send(
                Message(message=f"unknown character `{chr}`", path=path, span=span)
            )

            raise Error()

    except StopIteration:
        if not can_eof:
            point = head.get_point() - 1
            span = (point, point)

            msg.send(Message(message="unexpected end of file", path=path, span=span))

            raise Error()
        pass


class Lexer:
    def __init__(self, path, text):
        self.path = path
        self.head = peekable(tokenize(path, text))

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
