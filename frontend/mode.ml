type t = bool * bool (* mutable? , reference? *)

let create ?(mut = false) ?(reference = false) () = (mut, reference)
let is_mutable (m : t) = fst m
let is_reference (m : t) = snd m
