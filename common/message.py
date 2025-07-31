from typing import Optional
from dataclasses import dataclass
from enum import Enum
from collections import deque
from pathlib import Path

from common.sourcemap import Sourcemap, KNOWN_FILES


class Kind(Enum):
    WARNING = "warning"
    ERROR = "error"


class MarkKind(Enum):
    DIRECT = "direct"


class Message:
    def __init__(self, message, **kwargs):
        self.kind = Kind.ERROR
        self.message = message
        self.variables = kwargs
        self.highlights = []

    def add_point(self, path, point: int, kind: MarkKind = MarkKind.DIRECT, tag=""):
        point = ("point", kind, tag, path, point)
        self.highlights.append(point)
        return self

    def add_span(
        self, path, span: tuple[int, int], kind: MarkKind = MarkKind.DIRECT, tag=""
    ):
        point = ("span", kind, tag, path, span)
        self.highlights.append(point)
        return self

    def send(self):
        MESSAGES.append(self)

    def report(self):
        pass


MESSAGES = deque([])


def send(msg: Message):
    MESSAGES.append(msg)
