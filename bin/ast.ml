type path = Atom of string | Member of path * path

type expr =
  | Variable of path
  | Integer of int
  | Real of float
  | Unit
  | Bool of bool

type ty = Function of ty * ty | Named of path | UnitTy
type toplevel = FunctionDefinition of { name : string; ty : ty; value : expr }
