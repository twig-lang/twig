open Ir

exception TypeMismatch of Ty.t * Ty.t
exception ExpectedInteger of Ty.t
exception ExpectedFloat of Ty.t

type block_builder = {
  context : func ref;
  inner : block ref;
  add_terminator : terminator -> unit;
  add_value : value -> vref;
  get : unit -> block;
}

let create_block_builder context () =
  let inner = ref create_block in

  {
    context;
    inner;
    add_value =
      (fun value ->
        let index = IMap.cardinal !inner.values in
        inner := { !inner with values = IMap.add index value !inner.values };
        ValueRef (index, value.ty));
    add_terminator = (fun term -> inner := { !inner with terminator = term });
    get = (fun () -> !inner);
  }

type func_builder = {
  inner : func ref;
  add_block : block -> bref;
  build_block : unit -> block_builder;
  set_entry : bref -> unit;
  get : unit -> func;
}

let create_func_builder args ret () =
  let inner = ref @@ create_func args ret in

  {
    inner;
    add_block =
      (fun blk ->
        let index = IMap.cardinal !inner.blocks in
        inner := { !inner with blocks = IMap.add index blk !inner.blocks };
        BlockRef index);
    build_block = (fun () -> create_block_builder inner ());
    set_entry = (fun br -> inner := { !inner with entry = br });
    get = (fun () -> !inner);
  }

(* Block instructions *)

let kunit builder = builder.add_value @@ { instruction = Unit; ty = Ty.Unit }

let kint t k builder =
  if not @@ Ty.is_int t then raise @@ ExpectedInteger t;
  builder.add_value @@ { instruction = Int k; ty = t }

let kfloat t k builder =
  if not @@ Ty.is_float t then raise @@ ExpectedFloat t;
  builder.add_value @@ { instruction = Float k; ty = t }

let kbool k builder =
  builder.add_value @@ { instruction = Bool k; ty = Ty.Bool }

let add l r builder =
  let (ValueRef (_, lt)) = l in
  let (ValueRef (_, rt)) = r in

  if lt <> rt then raise @@ TypeMismatch (lt, rt);
  builder.add_value @@ { instruction = Add (lt, l, r); ty = lt }

(* Block terminators *)

let unreachable builder = builder.add_terminator Unreachable
let return value builder = builder.add_terminator @@ Return value
let jump target builder = builder.add_terminator @@ Jump target

let branch condition t f builder =
  builder.add_terminator @@ Branch (condition, t, f)
