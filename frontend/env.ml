(* inference environments *)
type variable = Variable of variable Ty.t option ref
type resolved = |
type expected = { return : variable Ty.t option; yield : variable Ty.t option }

(* this environment's "local context"*)
type t =
  | Root of { context : variable Tree.t; expect : expected }
  | Var of { super : t; name : string; mode : Mode.t; ty : variable Ty.t }
  | Vars of { super : t; binds : (Mode.t * variable Ty.t) Map.t }
  | Label of { super : t; name : string option; ty : variable Ty.t }

let create ?return ?yield context =
  let expect = { return; yield } in
  Root { context; expect }

let rec context = function
  | Root { context; _ } -> context
  | Var { super; _ } -> context super
  | Vars { super; _ } -> context super
  | Label { super; _ } -> context super

let rec expect = function
  | Root { expect; _ } -> expect
  | Var { super; _ } -> expect super
  | Vars { super; _ } -> expect super
  | Label { super; _ } -> expect super

let add_var ctx name mode ty = Var { super = ctx; name; mode; ty }
let add_vars ctx binds = Vars { super = ctx; binds }
let add_label ctx name ty = Label { super = ctx; name; ty }

let rec find_variable ctx vname =
  match ctx with
  | Root _ -> None
  | Var { super; name; ty; mode } ->
      if String.equal name vname then Some (mode, ty)
      else find_variable super vname
  | Vars { super; binds } -> (
      match Map.read_opt vname binds with
      | Some p -> Some p
      | None -> find_variable super vname)
  | Label { super; _ } -> find_variable super vname

let rec find_label ctx vname =
  match ctx with
  | Root _ -> None
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
