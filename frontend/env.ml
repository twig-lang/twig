(* inference environments *)
type variable = Variable of variable Ty.t option ref
type resolved = |
type expected = { return : variable Ty.t option; yield : variable Ty.t option }

(* this environment's "local context"*)
type t =
  | Root of { context : variable Tree.t; expect : expected }
  | Var of { super : t; name : string; mode : Mode.t; ty : variable Ty.t }
  | Vars of { super : t; binds : (Mode.t * variable Ty.t) Map.t }

let create ?return ?yield context =
  let expect = { return; yield } in
  Root { context; expect }

let rec context = function
  | Root { context; _ } -> context
  | Var { super; _ } -> context super
  | Vars { super; _ } -> context super

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

let add_var ctx name mode ty = Var { super = ctx; name; mode; ty }
let add_vars ctx binds = Vars { super = ctx; binds }

let rec expect = function
  | Root { expect; _ } -> expect
  | Var { super; _ } -> expect super
  | Vars { super; _ } -> expect super
