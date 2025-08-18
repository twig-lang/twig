import struct


class Bytestream:
    def __init__(self):
        self._inner = bytearray()

    def byte(self, byte: int):
        assert byte >= 0
        assert byte <= 255

        self._inner.append(byte)

    def bytes(self, bs: bytes):
        self._inner.append(bs)

    def integer(self, number: int, size=4, signed=False, endian="little"):
        endian_fmt = ({"little": "<", "big": ">"})[endian]

        size_fmt = (
            {
                1: ("B", "b"),
                2: ("H", "h"),
                4: ("I", "i"),
                8: ("L", "l"),
            }
        )[
            size
        ][1 if signed else 0]

        bs = struct.pack(endian_fmt + size_fmt, number)
        self._inner.extend(bs)

    def float(self, number: float, endian="little", size=4):
        endian_fmt = ({"little": "<", "big": ">"})[endian]

        size_fmt = (
            {
                4: "f",
                8: "d",
            }
        )[size]

        bs = struct.pack(endian_fmt + size_fmt, number)
        self._inner.extend(bs)

    def present(self) -> bytes:
        return bytes(self._inner)
