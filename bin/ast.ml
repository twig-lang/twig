type mode = Mode of { is_ref : bool; is_mut : bool }

type path = Member of path list | Atom of string
and ty = Named of path | UnitTy

and expr =
  | Variable of path
  | Integer of int
  | Real of float
  | Unit
  | Bool of bool
  | Block of stmt list

and stmt =
  | ExprStmt of expr
  | Let of { name : string; ty : ty option; mode : mode; value : expr }

let value is_mut = Mode { is_ref = false; is_mut }
let reference is_mut = Mode { is_ref = true; is_mut }

type call_arg =
  | CallArgument of {
      mode : mode;
      name : string;
      key : string option;
      ty : ty;
      default : expr option;
    }

type call_arglist = call_arg list

type toplevel =
  | FunctionDefinition of {
      name : string;
      arguments : call_arglist;
      ty : ty option;
      value : expr option;
    }
  | ConstantDefinition of { name : string; ty : ty; value : expr }
