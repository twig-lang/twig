open Util
module M = Stdlib.Map.Make (String)

exception Not_found of string

type 'a t = 'a M.t

let empty = M.empty
let of_list = M.of_list
let of_seq = M.of_seq
let to_list = M.to_list
let to_seq = M.to_seq
let create = M.add
let read_opt = M.find_opt

let read name env =
  try M.find name env with Stdlib.Not_found -> raise (Not_found name)

let update name up env = M.update name (Combinator.const (Some up)) env
let delete = M.remove
let iter f env = M.iter f env
let find_opt = read_opt
let find = read
let add = create
