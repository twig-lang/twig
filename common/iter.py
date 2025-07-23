from dataclasses import dataclass


class Peek:
    def __init__(self, gen):
        self.generator = gen
        self.peeked = None
        self.offset = 0
        self.peek_offset = 0

    def peek(self):
        if self.peeked is None:
            self.peeked = next(self.generator)
            self.peek_offset += 1

        return self.peeked

    def next(self):
        return self.__next__()

    def get_point(self):
        return self.offset

    def __next__(self):
        temp = self.peek()
        self.peeked = None
        self.offset = self.peek_offset
        return temp

    def __iter__(self):
        return self


def peekable(iter):
    return Peek(iter)
