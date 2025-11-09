(* inference environments *)
type variable = Variable of variable Ty.t option ref
type resolved = |

type expected = {
  mode : Mode.t;
  return : variable Ty.t option;
  yield : variable Ty.t option;
}

(* this environment's "local context"*)
type t =
  | Root of { expect : expected }
  | Context of { super : t; context : variable Tree.t }
  | Var of { super : t; name : string; mode : Mode.t; ty : variable Ty.t }
  | Vars of { super : t; binds : (Mode.t * variable Ty.t) Map.t }
  | Label of { super : t; name : string option; ty : variable Ty.t }

let create ?mode ?return ?yield () =
  let mode = Option.value ~default:(Mode.create ()) mode in
  let expect = { mode; return; yield } in
  Root { expect }

let rec context = function
  | Root _ -> None
  | Context { context; super } -> Some (super, context)
  | Var { super; _ } -> context super
  | Vars { super; _ } -> context super
  | Label { super; _ } -> context super

let rec expect = function
  | Root { expect; _ } -> expect
  | Context { super; _ } -> expect super
  | Var { super; _ } -> expect super
  | Vars { super; _ } -> expect super
  | Label { super; _ } -> expect super

let add_context from ctx = Context { super = ctx; context = from }
let add_var ctx name mode ty = Var { super = ctx; name; mode; ty }
let add_vars ctx binds = Vars { super = ctx; binds }
let add_label ctx name ty = Label { super = ctx; name; ty }

let rec find_variable ctx vname =
  match ctx with
  | Root _ -> None
  | Context { super; _ } -> find_variable super vname
  | Var { super; name; ty; mode } ->
      if String.equal name vname then Some (mode, ty)
      else find_variable super vname
  | Vars { super; binds } -> (
      try Some (Map.find vname binds)
      with Map.Not_found _ -> find_variable super vname)
  | Label { super; _ } -> find_variable super vname

let rec find_label ctx vname =
  match ctx with
  | Root _ -> None
  | Context { super; _ } -> find_label super vname
  | Var { super; _ } -> find_label super vname
  | Vars { super; _ } -> find_label super vname
  | Label { super; name; ty } -> (
      match (vname, name) with
      (* explicit name, named label *)
      | Some vname', Some name ->
          if String.equal vname' name then Some ty else find_label super vname
      (* implicit name, any label *)
      | None, _ -> Some ty
      (* explicit name, unnamed label *)
      | Some _, None -> find_label super vname)

let rec find_toplevel predicate = function
  | Root _ -> failwith "could not find a toplevel"
  | Context { super; context } -> (
      try predicate context with _ -> find_toplevel predicate super)
  | Var { super; _ } -> find_toplevel predicate super
  | Vars { super; _ } -> find_toplevel predicate super
  | Label { super; _ } -> find_toplevel predicate super
