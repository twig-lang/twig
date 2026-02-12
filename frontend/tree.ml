type ty_definition = { ty : Ty.t }
type fn_signature = { return : Ty.t; parameters : Expr.parameter_map }
type const_signature = { ty : Ty.t }
type fn_definition = { s : fn_signature; value : Expr.t }
type const_definition = { s : const_signature; value : Expr.t }

type sub_signature = {
  return : Ty.t;
  mode : Mode.t;
  parameters : Expr.parameter_map;
}

type sub_definition = { s : sub_signature; value : Expr.t }

type definition =
  | TypeDefinition of string * ty_definition
  | FnDefinition of string * fn_definition
  | FnDeclaration of string * fn_signature
  | ConstDefinition of string * const_definition
  | ConstDeclaration of string * const_signature
  | SubDeclaration of string * sub_signature
  | SubDefinition of string * sub_definition

type t = {
  parent : t option;
  fn_definitions : fn_definition Map.t;
  const_definitions : const_definition Map.t;
  ty_definitions : ty_definition Map.t;
  modules : t Map.t;
  fn_signatures : fn_signature Map.t;
  const_signatures : const_signature Map.t;
  sub_signatures : sub_signature Map.t;
  sub_definitions : sub_definition Map.t;
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
      let ty_definitions = Map.add name d m.ty_definitions in
      { m with ty_definitions }
  | FnDeclaration (name, s) ->
      let fn_signatures = Map.add name s m.fn_signatures in
      { m with fn_signatures }
  | FnDefinition (name, d) ->
      let m = add m (FnDeclaration (name, d.s)) in
      (* TODO: Check and maybe populate the function signature? *)
      let fn_definitions = Map.add name d m.fn_definitions in
      { m with fn_definitions }
  | ConstDefinition (name, d) ->
      let m = add m (ConstDeclaration (name, d.s)) in
      (* Also the signature here? *)
      let const_definitions = Map.add name d m.const_definitions in
      { m with const_definitions }
  | ConstDeclaration (name, s) ->
      let const_signatures = Map.add name s m.const_signatures in
      { m with const_signatures }
  | SubDeclaration (name, s) ->
      let sub_signatures = Map.add name s m.sub_signatures in
      { m with sub_signatures }
  | SubDefinition (name, d) ->
      let m = add m (SubDeclaration (name, d.s)) in
      (* TODO: Check and maybe populate the function signature? *)
      let sub_definitions = Map.add name d m.sub_definitions in
      { m with sub_definitions }

let get_module p m =
  match p with Path.Atom a -> (a, m) | _ -> failwith "unsupported path!"

let get_fnsig p m =
  let a, m = get_module p m in
  Map.find a m.fn_signatures

let get_subsig p m =
  let a, m = get_module p m in
  Map.find a m.sub_signatures

let get_ksig p m =
  let a, m = get_module p m in
  Map.find a m.const_signatures

let get_ty p m =
  let a, m = get_module p m in
  Map.find a m.ty_definitions

let get_mod p m =
  match p with
  | Path.Atom a -> Map.find a m.modules
  | _ -> failwith "unsupported path!"
