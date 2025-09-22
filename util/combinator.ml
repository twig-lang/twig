(* Random combinators *)

let id x = x
let const _ = id
let ( <@@> ) f g = fun x -> f (g x)
let flip f = fun x y -> f y x
