type 'tv ty_definition = { ty : 'tv Ty.t }
type 'tv fn_signature = { return : 'tv Ty.t; arguments : 'tv Expr.param_list }
type 'tv const_signature = { ty : 'tv Ty.t }
type 'tv fn_definition = { s : 'tv fn_signature; value : 'tv Expr.t }
type 'tv const_definition = { s : 'tv const_signature; value : 'tv Expr.t }

type 'tv definition =
  | FnDefinition of string * 'tv fn_definition
  | FnDeclaration of string * 'tv fn_signature
  | ConstDefinition of string * 'tv const_definition

type 'tv t = {
  parent : 'tv t option;
  fn_definitions : 'tv fn_definition Env.t;
  const_definitions : 'tv const_definition Env.t;
  types : 'tv ty_definition Env.t;
  modules : 'tv t Env.t;
  fn_signatures : 'tv fn_signature Env.t;
  const_signatures : 'tv const_signature Env.t;
}

let empty =
  Env.
    {
      parent = None;
      fn_definitions = empty;
      const_definitions = empty;
      types = empty;
      modules = empty;
      fn_signatures = empty;
      const_signatures = empty;
    }

let add m def =
  match def with
  | FnDeclaration (name, s) ->
      let fn_signatures = Env.create name s m.fn_signatures in
      { m with fn_signatures }
  | FnDefinition (name, d) ->
      (* TODO: Check and maybe populate the function signature? *)
      let fn_definitions = Env.create name d m.fn_definitions in
      { m with fn_definitions }
  | ConstDefinition (name, d) ->
      (* Also the signature here? *)
      let const_definitions = Env.create name d m.const_definitions in
      { m with const_definitions }

let rec get_rec_module p m =
  match p with
  | Path.Atom a -> (a, m)
  | Path.Member (p, a) ->
      let a', m = get_rec_module p m in
      let m = Env.read a' m.modules in
      (a, m)
  | _ -> failwith "unsupported path!"

let get_fnsig p m =
  let a, m = get_rec_module p m in
  Env.read a m.fn_signatures

let get_ksig p m =
  let a, m = get_rec_module p m in
  Env.read a m.const_signatures

let get_ty p m =
  let a, m = get_rec_module p m in
  Env.read a m.types
