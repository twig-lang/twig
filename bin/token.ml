type tag =
  (* "ignored" tags *)
  | Eof
  | Whitespace
  | Comment
  (* "primitive" tags *)
  | Identifier
  | Integer
  | Real
  | String
  | Char
  | Bool
  (* brace pairs *)
  | LParen
  | RParen
  | LBrac
  | RBrac
  | LCurl
  | RCurl
  (* operator "keywords" *)
  | Dot (* . *)
  | Comma (* , *)
  | Colon (* : *)
  | Semicolon (* ; *)
  | Equal (* = *)
  | Bang (* ! *)
  | Question (* ? *)
  | Amp (* & *)
  | Bar (* | *)
  (* Keywords *)
  | KLet
  | KIn
  | KFn
  | KMut
  | KSub
  | KIf
  | KElse
  | KWhile
  | KLoop
  | KMatch
  | KType
  | KEnum
  | KStruct
  | KUnion
  | KUnsafe
  | KConst
  | KMod
  | KReturn
  | KYield
  | KBreak
  | KContinue

type data =
  | Str of string
  | Int of int
  | Float of float
  | Char of Uchar.t
  | Bool of bool

type t = tag * data option * Position.t
