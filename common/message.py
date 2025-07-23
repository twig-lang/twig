from typing import Optional
from dataclasses import dataclass
from enum import Enum
from collections import deque
from pathlib import Path

from common.sourcemap import Sourcemap, KNOWN_FILES


class Kind(Enum):
    WARNING = "warning"
    ERROR = "error"


@dataclass
class Message:
    message: str
    path: Path
    span: tuple[int, int]
    kind: Kind = Kind.ERROR

    def report(self):
        map = KNOWN_FILES[self.path]

        print(f"{self.kind.name}: {self.message}")
        b, e = self.span
        map.report(b, e)


MESSAGES = deque([])


def send(msg: Message):
    MESSAGES.append(msg)
