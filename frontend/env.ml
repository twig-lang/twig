open Util
module M = Map.Make (String)

exception Not_found of string

type 'a t = { bindings : 'a M.t }

let empty = { bindings = M.empty }

let create name item env =
  let bindings = M.add name item env.bindings in
  { bindings }

let read name env =
  try M.find name env.bindings with Stdlib.Not_found -> raise (Not_found name)

let update name up env =
  let bindings = M.update name (Combinator.const (Some up)) env.bindings in
  { bindings }

let delete name env =
  let bindings = M.update name (Combinator.const None) env.bindings in
  { bindings }

let iter f env = M.iter f env.bindings
