type 'tv ty_definition = { ty : 'tv Ty.t }
type 'tv fn_signature = { return : 'tv Ty.t; arguments : 'tv Expr.param_list }
type 'tv const_signature = { ty : 'tv Ty.t }
type 'tv fn_definition = { s : 'tv fn_signature; value : 'tv Expr.t }
type 'tv const_definition = { s : 'tv const_signature; value : 'tv Expr.t }

type 'tv sub_signature = {
  return : 'tv Ty.t;
  mode : Mode.t;
  arguments : 'tv Expr.param_list;
}

type 'tv sub_definition = { s : 'tv sub_signature; value : 'tv Expr.t }

type 'tv definition =
  | TypeDefinition of string * 'tv ty_definition
  | FnDefinition of string * 'tv fn_definition
  | FnDeclaration of string * 'tv fn_signature
  | ConstDefinition of string * 'tv const_definition
  | ConstDeclaration of string * 'tv const_signature
  | SubDeclaration of string * 'tv sub_signature
  | SubDefinition of string * 'tv sub_definition

type 'tv t = {
  parent : 'tv t option;
  fn_definitions : 'tv fn_definition Map.t;
  const_definitions : 'tv const_definition Map.t;
  ty_definitions : 'tv ty_definition Map.t;
  modules : 'tv t Map.t;
  fn_signatures : 'tv fn_signature Map.t;
  const_signatures : 'tv const_signature Map.t;
  sub_signatures : 'tv sub_signature Map.t;
  sub_definitions : 'tv sub_definition Map.t;
}

let empty =
  Map.
    {
      parent = None;
      fn_definitions = empty;
      const_definitions = empty;
      ty_definitions = empty;
      modules = empty;
      fn_signatures = empty;
      const_signatures = empty;
      sub_signatures = empty;
      sub_definitions = empty;
    }

let rec add m def =
  match def with
  | TypeDefinition (name, d) ->
      let ty_definitions = Map.create name d m.ty_definitions in
      { m with ty_definitions }
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
  | SubDeclaration (name, s) ->
      let sub_signatures = Map.create name s m.sub_signatures in
      { m with sub_signatures }
  | SubDefinition (name, d) ->
      let m = add m (SubDeclaration (name, d.s)) in
      (* TODO: Check and maybe populate the function signature? *)
      let sub_definitions = Map.create name d m.sub_definitions in
      { m with sub_definitions }

let get_module p m =
  match p with Path.Atom a -> (a, m) | _ -> failwith "unsupported path!"

let get_fnsig p m =
  let a, m = get_module p m in
  Map.read a m.fn_signatures

let get_subsig p m =
  let a, m = get_module p m in
  Map.read a m.sub_signatures

let get_ksig p m =
  let a, m = get_module p m in
  Map.read a m.const_signatures

let get_ty p m =
  let a, m = get_module p m in
  Map.read a m.ty_definitions
