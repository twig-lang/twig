from enum import Enum
from dataclasses import dataclass
from typing import Optional, Any


class Tag(Enum):
    # "base" tags
    Int = "an integer"
    Float = "a float"
    Identifier = "an identifier"
    Char = "a character"
    String = "a string"
    # keywords
    KwFunction = "`function`"
    KwBegin = "`begin`"
    KwEnd = "`end`"
    KwLoop = "`loop`"
    KwWhile = "`while`"
    KwDo = "`do`"
    KwIf = "`if`"
    KwThen = "`then`"
    KwElse = "`else`"
    KwYield = "`yield`"
    KwBreak = "`break`"
    KwContinue = "`continue`"
    KwReturn = "`return`"
    KwMut = "`mut`"
    KwSubscript = "`subscript`"
    KwDefer = "`defer`"
    KwImport = "`import`"
    KwWith = "`with`"
    KwModule = "`module`"
    KwType = "`type`"
    KwRecord = "`record`"
    KwUnion = "`union`"
    KwVariant = "`variant`"
    KwEnum = "`enum`"
    KwMatch = "`match`"
    KwCase = "`case`"
    KwLet = "`let`"
    KwIn = "`in`"
    KwCast = "`cast`"
    KwSet = "`set`"
    # operators
    OpAdd = "`+`"
    OpSub = "`-`"
    OpDiv = "`/`"
    OpMul = "`*`"
    OpMod = "`%`"
    OpShr = "`>>`"
    OpShl = "`<<`"
    OpLt = "`<`"
    OpGt = "`>`"
    OpNotEq = "`!=`"
    OpNot = "`!`"
    OpAndthen = "`&&`"
    OpOrelse = "`||`"
    OpAnd = "`&`"
    OpOr = "`|`"
    OpEq = "`=`"
    OpEqLt = "`=<`"
    OpEqGt = "`=>`"
    OpXor = "`^`"
    # punctuation
    PDot = "`.`"
    PComma = "`,`"
    PSemicolon = "`;`"
    PColon = "`:`"
    PLParen = "`(`"
    PRParen = "`)`"
    PLBracket = "`[`"
    PRBracket = "`]`"
    PHash = "`#`"

    def _make_kwtab():
        tab = {}

        for key, tag in Tag.__members__.items():
            if key.startswith("Kw"):
                tab[tag.value[1:-1]] = tag

        return tab


KEYWORDS = Tag._make_kwtab()

# 1-char long symbols.
# {} are reserved for comments.
SYM_1 = {
    "+": Tag.OpAdd,
    "-": Tag.OpSub,
    "/": Tag.OpDiv,
    "*": Tag.OpMul,
    "%": Tag.OpMod,
    ";": Tag.PSemicolon,
    ".": Tag.PDot,
    ",": Tag.PComma,
    ":": Tag.PColon,
    "^": Tag.OpXor,
    "(": Tag.PLParen,
    ")": Tag.PRParen,
    "[": Tag.PLBracket,
    "]": Tag.PRBracket,
    "#": Tag.PHash,
}

# 2-char long symbols.
SYM_2 = {
    ">": (Tag.OpGt, {">": Tag.OpShr}),
    "<": (Tag.OpLt, {"<": Tag.OpShl}),
    "!": (Tag.OpNot, {"=": Tag.OpNotEq}),
    "&": (Tag.OpAnd, {"&": Tag.OpAndthen}),
    "|": (Tag.OpOr, {"|": Tag.OpOrelse}),
    "=": (Tag.OpEq, {"<": Tag.OpEqLt, ">": Tag.OpEqGt}),
}


@dataclass
class Token:
    tag: Tag
    span: tuple[int, int]
    data: Optional[Any] = None
