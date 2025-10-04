type t = bool * bool (* mutable? , reference? *)

let create ?(mut = false) ?(reference = false) () = (mut, reference)
let is_mutable (m : t) = fst m
let is_reference (m : t) = snd m
let implies a c = if a then c else true

(* a "compatibility" between modes *)
let ( <: ) checked reference =
  snd checked == fst reference && implies (fst reference) (fst checked)

let ( >: ) reference checked = checked <: reference
