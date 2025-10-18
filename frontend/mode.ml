type mutability = Immutable | Mutable
type sharing = Value | Reference
type t = Mode of mutability * sharing

let is_mutable (Mode (m, _)) = m == Mutable
let is_reference (Mode (_, s)) = s == Reference
let implies a c = if a then c else true

(* a "compatibility" between modes *)
let ( <: ) checked reference =
  is_reference checked == is_reference reference
  && implies (is_mutable reference) (is_mutable checked)

let ( >: ) reference checked = checked <: reference

let fmt_mutability f m =
  let m = match m with Immutable -> "immutable" | Mutable -> "mutable" in
  Printf.fprintf f "%s" m

let fmt_sharing f s =
  let s = match s with Value -> "value" | Reference -> "reference" in
  Printf.fprintf f "%s" s

let fmt c (Mode (m, s)) =
  Printf.fprintf c "Mode(%a, %a)" fmt_mutability m fmt_sharing s
