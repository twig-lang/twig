type path = Atom of string | Member of path * path

type expr =
  | Variable of path
  | Integer of int
  | Real of float
  | Unit
  | Bool of bool

type ty = Named of path | UnitTy

type call_arg =
  | CallArgument of {
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
