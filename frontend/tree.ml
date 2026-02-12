type fn_signature = {
  name : string;
  return : Ty.t;
  parameters : Expr.parameter_map;
}

type const_signature = { name : string; ty : Ty.t }

type sub_signature = {
  name : string;
  return : Ty.t;
  mode : Mode.t;
  parameters : Expr.parameter_map;
}

type fn_definition = { signature : fn_signature; name : string; value : Expr.t }

type const_definition = {
  signature : const_signature;
  name : string;
  value : Expr.t;
}

type sub_definition = {
  signature : sub_signature;
  name : string;
  value : Expr.t;
}

type ty_definition = { name : string; ty : Ty.t }

type definition =
  | TypeDefinition of ty_definition
  | FnDefinition of fn_definition
  | FnDeclaration of fn_signature
  | ConstDefinition of const_definition
  | ConstDeclaration of const_signature
  | SubDeclaration of sub_signature
  | SubDefinition of sub_definition

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
  | TypeDefinition d ->
      let ty_definitions = Map.add d.name d m.ty_definitions in
      { m with ty_definitions }
  | FnDeclaration s ->
      let fn_signatures = Map.add s.name s m.fn_signatures in
      { m with fn_signatures }
  | FnDefinition d ->
      let m = add m (FnDeclaration d.signature) in
      let fn_definitions = Map.add d.name d m.fn_definitions in
      { m with fn_definitions }
  | ConstDefinition d ->
      let m = add m (ConstDeclaration d.signature) in
      (* Also the signature here? *)
      let const_definitions = Map.add d.name d m.const_definitions in
      { m with const_definitions }
  | ConstDeclaration s ->
      let const_signatures = Map.add s.name s m.const_signatures in
      { m with const_signatures }
  | SubDeclaration s ->
      let sub_signatures = Map.add s.name s m.sub_signatures in
      { m with sub_signatures }
  | SubDefinition d ->
      let m = add m (SubDeclaration d.signature) in
      let sub_definitions = Map.add d.name d m.sub_definitions in
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
