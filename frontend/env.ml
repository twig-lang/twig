open Util
module M = Map.Make (String)

type 'a t = { bindings : 'a M.t }

let empty = { bindings = M.empty }

let create name item env =
  let bindings = M.add name item env.bindings in
  { bindings }

let read name env = M.find name env.bindings

let update name up env =
  let bindings =
    M.update name
      Combinator.(Option.some <@@> const up <@@> Option.get)
      env.bindings
  in
  { bindings }

let delete name env =
  let bindings = M.update name (Combinator.const None) env.bindings in
  { bindings }
