type path = Atom of string | Member of path * path

type expr =
  | Variable of path
  | Integer of int
  | Real of float
  | Unit
  | Bool of bool

type ty = Named of path | UnitTy
type mode = Mode of { is_ref : bool; is_mut : bool }

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
