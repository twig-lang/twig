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

type value = Value of { instruction : instruction; ty : Ty.t }

type terminator =
  | Unreachable
  | Return of vref
  | Jump of bref
  | Branch of vref * bref * bref

type block = Block of { values : value IMap.t; terminator : terminator }

type func =
  | Func of {
      arguments : (string * Ty.t) list;
      return : Ty.t;
      blocks : block IMap.t;
    }

type block_op = SetTerminator of terminator | SetValue of int * value
type func_op = AddBlock of int * block | RequestBlock

let create_block = Block { values = IMap.empty; terminator = Unreachable }

let create_func arguments return =
  Func { arguments; return; blocks = IMap.empty }

(* let* is for blocks, let+ is for funcs *)

let apply_block_op (Block blk) = function
  | SetTerminator t -> Block { blk with terminator = t }
  | SetValue (k, v) ->
      let vals = IMap.add k v blk.values in
      Block { blk with values = vals }

let apply_func_op (Func fn) = function
  | AddBlock (k, v) ->
      let bs = IMap.add k v fn.blocks in
      Func { fn with blocks = bs }
  | RequestBlock ->
      let k = IMap.cardinal fn.blocks in
      let v = create_block in
      let bs = IMap.add k v fn.blocks in
      Func { fn with blocks = bs }

let ( let* ) o t =
 fun b ->
  let b = apply_block_op b o in
  t b

let ( let+ ) o t =
 fun f ->
  let f = apply_func_op f o in
  t f

type block_builder = block -> block
type func_builder = func -> func

(* Blocks... *)
let set_terminator term =
  let* b = SetTerminator term in
  b

let add_value v =
 fun (Block b) ->
  let k = IMap.cardinal b.values in
  let* b = SetValue (k, v) in
  b

let unreachable () = set_terminator Unreachable
let return v = set_terminator @@ Return v
let jump b = set_terminator @@ Jump b
let branch c t f = set_terminator @@ Branch (c, t, f)

let kunit () =
  add_value (Value { instruction = Unit; ty = Ty.Primitive Ty.Primitive.Unit })

let kint t i = add_value (Value { instruction = Int i; ty = t })
let kfloat t f = add_value (Value { instruction = Float f; ty = t })

let kbool b =
  add_value
    (Value { instruction = Bool b; ty = Ty.Primitive Ty.Primitive.Bool })

let add l r =
  let (ValueRef (_, t)) = l in
  add_value (Value { instruction = Add (t, l, r); ty = t })

(* Functions... *)
