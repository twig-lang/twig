type t = { byte_from : int; byte_to : int }

let create f t = { byte_from = f; byte_to = t }
let set_from p f = { p with byte_from = f }
let set_to p t = { p with byte_to = t }
