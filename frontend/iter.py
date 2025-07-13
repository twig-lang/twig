from dataclasses import dataclass


@dataclass
class Point:
    line: int = 0
    column: int = 0
    offset: int = 0

    def step(self, chr):
        self.column += 1
        self.offset += 1

        if chr == "\n":
            self.line += 1
            self.column = 0

    def __repr__(self):
        return f"({self.line}:{self.column}@{self.offset})"


class Peek:
    def __init__(self, gen):
        self.generator = gen
        self.peeked = None
        self.point = Point()
        self.peek_point = Point()

    def peek(self):
        if self.peeked is None:
            self.peeked = next(self.generator)
            self.peek_point.step(self.peeked)

        return self.peeked

    def next(self):
        return self.__next__()

    def get_point(self):
        return self.point

    def __next__(self):
        temp = self.peek()
        self.peeked = None
        self.point = self.peek_point
        return temp

    def __iter__(self):
        return self


def peekable(iter):
    return Peek(iter)
