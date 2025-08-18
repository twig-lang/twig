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


_MESSAGES = deque([])


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
        _MESSAGES.append(self)

    def report(self):
        pass


def _report_one(msg: Message):
    print(f"{msg.kind.value}: {msg.message}")

    for highlight in msg.highlights:
        print(f" %{highlight}")

    for var, value in msg.variables.items():
        print(f" ${repr(var)}={repr(value)}")


def report_all():
    errors = 0
    warnings = 0

    while len(_MESSAGES) > 0:
        message = _MESSAGES.popleft()

        if message.kind == Kind.ERROR:
            errors += 1

        if message.kind == Kind.WARNING:
            warnings += 1

        _report_one(message)

    if errors != 0 or warnings != 0:
        print(f"{errors} error(s), {warnings} warning(s) total.")

    return errors == 0
