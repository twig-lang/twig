type mutability = Immutable | Mutable
type sharing = Value | Reference
type t = mutability * sharing

let is_mutable m = fst m == Mutable
let is_reference m = snd m == Reference
let implies a c = if a then c else true

(* a "compatibility" between modes *)
let ( <: ) checked reference =
  snd checked == fst reference && implies (fst reference) (fst checked)

let ( >: ) reference checked = checked <: reference
