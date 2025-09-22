(* Haskell's Arrow class, but only for functions *)

open Combinator

let ( >>> ) f g = g <@@> f
let ( <<< ) f g = f <@@> g
let ( &&& ) f g = fun x -> (f x, g x)
let ( ||| ) f g = fun x -> Either.fold ~left:f ~right:g x
