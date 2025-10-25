type 'tv ty_definition = { ty : 'tv Ty.t }
type 'tv fn_signature = { return : 'tv Ty.t; arguments : 'tv Expr.param_list }
type 'tv const_signature = { ty : 'tv Ty.t }
type 'tv fn_definition = { s : 'tv fn_signature; value : 'tv Expr.t }
type 'tv const_definition = { s : 'tv const_signature; value : 'tv Expr.t }

type 'tv definition =
  | FnDefinition of string * 'tv fn_definition
  | FnDeclaration of string * 'tv fn_signature
  | ConstDefinition of string * 'tv const_definition
  | ConstDeclaration of string * 'tv const_signature

type 'tv t = {
  parent : 'tv t option;
  fn_definitions : 'tv fn_definition Map.t;
  const_definitions : 'tv const_definition Map.t;
  types : 'tv ty_definition Map.t;
  modules : 'tv t Map.t;
  fn_signatures : 'tv fn_signature Map.t;
  const_signatures : 'tv const_signature Map.t;
}

let empty =
  Map.
    {
      parent = None;
      fn_definitions = empty;
      const_definitions = empty;
      types = empty;
      modules = empty;
      fn_signatures = empty;
      const_signatures = empty;
    }

let rec add m def =
  match def with
  | FnDeclaration (name, s) ->
      let fn_signatures = Map.create name s m.fn_signatures in
      { m with fn_signatures }
  | FnDefinition (name, d) ->
      let m = add m (FnDeclaration (name, d.s)) in
      (* TODO: Check and maybe populate the function signature? *)
      let fn_definitions = Map.create name d m.fn_definitions in
      { m with fn_definitions }
  | ConstDefinition (name, d) ->
      let m = add m (ConstDeclaration (name, d.s)) in
      (* Also the signature here? *)
      let const_definitions = Map.create name d m.const_definitions in
      { m with const_definitions }
  | ConstDeclaration (name, s) ->
      let const_signatures = Map.create name s m.const_signatures in
      { m with const_signatures }

let rec get_rec_module p m =
  match p with
  | Path.Atom a -> (a, m)
  | Path.Member (p, a) ->
      let a', m = get_rec_module p m in
      let m = Map.read a' m.modules in
      (a, m)
  | _ -> failwith "unsupported path!"

let get_fnsig p m =
  let a, m = get_rec_module p m in
  Map.read a m.fn_signatures

let get_ksig p m =
  let a, m = get_rec_module p m in
  Map.read a m.const_signatures

let get_ty p m =
  let a, m = get_rec_module p m in
  Map.read a m.types
