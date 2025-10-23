type t = Atom of string | Call of t * t list | Member of t * string

let rec equal l r =
  match (l, r) with
  | Atom a, Atom b -> String.equal a b
  | Member (aa, ad), Member (ba, bd) -> equal aa ba && String.equal ad bd
  | Call (lp, la), Call (rp, ra) -> equal lp rp && List.for_all2 equal la ra
  | _ -> false

let rec fmt f =
  let open Printf in
  function
  | Atom a -> fprintf f "%s" a
  | Member (p, a) ->
      fmt f p;
      fprintf f ".%s" a
  | Call (p, _a) ->
      fmt f p;
      fprintf f "!(...)"
