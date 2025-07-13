from typing import Optional
from dataclasses import dataclass
from contextlib import contextmanager
from enum import Enum

from frontend.sourcemap import Sourcemap


class ErrorKind(Enum):
    WARNING = "warning"
    ERROR = "error"


@dataclass
class Error(Exception):
    message: str
    kind: ErrorKind = ErrorKind.ERROR
    span: Optional[tuple[int, int]] = None


@contextmanager
def span_errors(span: tuple[int, int]):
    try:
        yield None
    except Error as err:
        if err.span is None:
            err.span = span

        raise err
