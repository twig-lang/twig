let id x = x
let const _ = id
let ( <@@> ) f g = fun x -> f (g x)
