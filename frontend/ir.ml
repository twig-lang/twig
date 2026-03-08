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
  arguments : (string * Ty.t) list;
  return : Ty.t;
  blocks : block IMap.t;
}

let create_block = { values = IMap.empty; terminator = Unreachable }
let create_func arguments return = { arguments; return; blocks = IMap.empty }
