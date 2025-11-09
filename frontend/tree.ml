module M (S : Stage.S) = struct
  module Expression = Expr.M (S)

  type ty_definition = { defined : S.ty }
  type fn_signature = { return : S.ty; arguments : Expression.parameter_list }
  type fn_definition = { s : fn_signature; value : Expression.t }

  type sub_signature = {
    yield : S.ty;
    mode : Mode.t;
    arguments : Expression.parameter_list;
  }

  type sub_definition = { s : sub_signature; value : Expression.t }
  type const_signature = { ty : S.ty }
  type const_definition = { s : const_signature; value : Expression.t }

  type t = {
    parent : t option;
    children : t Map.t;
    ty_definitions : ty_definition Map.t;
    fn_signatures : fn_signature Map.t;
    fn_definitions : fn_definition Map.t;
    const_signatures : const_signature Map.t;
    const_definitions : const_definition Map.t;
    sub_signatures : sub_signature Map.t;
    sub_definitions : sub_definition Map.t;
  }

  let empty =
    Map.
      {
        parent = None;
        children = empty;
        ty_definitions = empty;
        fn_signatures = empty;
        fn_definitions = empty;
        const_signatures = empty;
        const_definitions = empty;
        sub_signatures = empty;
        sub_definitions = empty;
      }

  type toplevel =
    | Type_definition of string * ty_definition
    | Fn_definition of string * fn_definition
    | Fn_declaration of string * fn_signature
    | Const_definition of string * const_definition
    | Const_declaration of string * const_signature
    | Sub_declaration of string * sub_signature
    | Sub_definition of string * sub_definition

  (* TODO: for every definition check and add their signature *)
  let rec add m def =
    match def with
    | Type_definition (name, d) ->
        let ty_definitions = Map.add name d m.ty_definitions in
        { m with ty_definitions }
    | Fn_declaration (name, s) ->
        let fn_signatures = Map.add name s m.fn_signatures in
        { m with fn_signatures }
    | Fn_definition (name, d) ->
        let m = add m (Fn_declaration (name, d.s)) in
        let fn_definitions = Map.add name d m.fn_definitions in
        { m with fn_definitions }
    | Const_declaration (name, s) ->
        let const_signatures = Map.add name s m.const_signatures in
        { m with const_signatures }
    | Const_definition (name, d) ->
        let m = add m (Const_declaration (name, d.s)) in
        let const_definitions = Map.add name d m.const_definitions in
        { m with const_definitions }
    | Sub_declaration (name, s) ->
        let sub_signatures = Map.add name s m.sub_signatures in
        { m with sub_signatures }
    | Sub_definition (name, d) ->
        let m = add m (Sub_declaration (name, d.s)) in
        let sub_definitions = Map.add name d m.sub_definitions in
        { m with sub_definitions }
end

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
