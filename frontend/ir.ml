module IMap = Stdlib.Map.Make (Int)

type err =
  | ExpectedInteger of Ty.t
  | ExpectedReal of Ty.t
  | TypeMismatch of Ty.t * Ty.t

type vref = ValueRef of int * Ty.t
type bref = BlockRef of int

type instruction =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Add of Ty.t * vref * vref

type value = { instruction : instruction; ty : Ty.t }

type terminator =
  | Unreachable
  | Return of vref
  | Jump of bref
  | Branch of vref * bref * bref

type block = { values : value IMap.t; terminator : terminator }

type func = {
  parameters : (string * Ty.t) list;
  return : Ty.t;
  blocks : block IMap.t;
  entry : bref;
}

type m = { funcs : func Map.t }

let create_block = { values = IMap.empty; terminator = Unreachable }
let create_func parameters return =
  { parameters; return; blocks = IMap.empty; entry = BlockRef 0 }
