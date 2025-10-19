type mutability = Immutable | Mutable
type sharing = Value | Reference
type t = Mode of mutability * sharing

let is_mutable (Mode (m, _)) = m == Mutable
let is_reference (Mode (_, s)) = s == Reference
let implies a c = if a then c else true

(* a "compatibility" between modes *)
let ( <: ) parameter argument =
  (* both modes are either values or references at the same time *)
  let equal_sharing = is_reference parameter == is_reference argument in

  (* immutable modes are a subtype of mutable modes, but not vice versa *)
  let par_mut_implies_arg_mut =
    implies (is_mutable parameter) (is_mutable argument)
  in

  (* we only care about parameter mutability if both are references *)
  equal_sharing && implies (is_reference parameter) par_mut_implies_arg_mut

let ( >: ) = Util.Combinator.flip ( <: )

let fmt_mutability f m =
  let m = match m with Immutable -> "immutable" | Mutable -> "mutable" in
  Printf.fprintf f "%s" m

let fmt_sharing f s =
  let s = match s with Value -> "value" | Reference -> "reference" in
  Printf.fprintf f "%s" s

let fmt c (Mode (m, s)) =
  Printf.fprintf c "Mode(%a, %a)" fmt_mutability m fmt_sharing s
