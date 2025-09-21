type argument = Argument of t
and t = Atom of string | Call of t * argument list | Member of t * string

let rec equal l r =
  let equal_argument l r =
    match (l, r) with Argument l, Argument r -> equal l r
  in
  match (l, r) with
  | Atom a, Atom b -> String.equal a b
  | Member (aa, ad), Member (ba, bd) -> equal aa ba && String.equal ad bd
  | Call (lp, la), Call (rp, ra) ->
      equal lp rp && List.for_all2 equal_argument la ra
  | _ -> false
