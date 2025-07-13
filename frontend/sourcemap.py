from pathlib import Path


class Sourcemap:
    def __init__(self, path: Path, text):
        self.text = text
        self.path = path
        self.lines = text.split("\n")
        self.offsets = []

        offset = 0
        for line in self.lines:
            self.offsets.append((offset, offset + len(line) + 1))
            offset += len(line) + 1

    def get_line_column_from(self, offset: int) -> tuple[int, int]:
        line = 0

        for begin, end in self.offsets:
            if offset in range(begin, end):
                column = offset - begin
                return (line, column)

            line += 1

        raise IndexError(offset, len(self.text))

    def report(self, begin: int, end: int):
        beg_line, beg_col = self.get_line_column_from(begin)
        end_line, end_col = self.get_line_column_from(end)

        print(f"[{self.path} {beg_line+1}:{beg_col+1}]")
        for line in range(beg_line, end_line + 1):
            print(f"{line+1 : 5} | {self.lines[line]}")


KNOWN_FILES: dict[Path, Sourcemap] = {}


def add_file(path: Path) -> str:
    text = open(path).read()

    map = Sourcemap(path, text)
    KNOWN_FILES[path] = map

    return text
