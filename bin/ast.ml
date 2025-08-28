type mode = Mode of { is_ref : bool; is_mut : bool }

type path = Member of path list | Atom of string
and ty = Named of path | UnitTy
and fn_arg = FnArgument of { key : string option; mode : mode; value : expr }

and message =
  (* name (args...)? : tail? *)
  | FnMessage of { name : path; args : fn_arg list; tail : expr option }

and expr =
  | Variable of path
  | Integer of int
  | Real of float
  | Unit
  | Bool of bool
  | Block of expr list
  (* NOTE: typed as () *)
  | Let of { name : string; ty : ty option; mode : mode; value : expr }
  (* callee (args...) *)
  | FnCall of { callee : expr; args : fn_arg list }
  (* expr message* *)
  | Send of { recv : expr; msg : message }

let value is_mut = Mode { is_ref = false; is_mut }
let reference is_mut = Mode { is_ref = true; is_mut }

type fn_parameter =
  | FnParameter of {
      mode : mode;
      name : string;
      key : string option;
      ty : ty;
      default : expr option;
    }

type fn_parlist = fn_parameter list

type toplevel =
  | FunctionDefinition of {
      name : string;
      parameters : fn_parlist;
      ty : ty option;
      value : expr option;
    }
  | ConstantDefinition of { name : string; ty : ty; value : expr }
