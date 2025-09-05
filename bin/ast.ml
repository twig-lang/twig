type mode = Mode of { is_ref : bool; is_mut : bool }
type yields = Returns | YieldIf | YieldWhile

type path =
  | Member of path list
  | Atom of string
  | Call of { name : string; args : path list }

and struct_member = StructMember of { name : string; ty : ty }

and enum_member =
  | EnumMember of { name : string }
  | EnumMemberArgs of { name : string; args : ty list }

and ty =
  | Named of path
  | UnitTy
  | StructTy of { members : struct_member list }
  | UnionTy of { members : struct_member list }
  | EnumTy of { members : enum_member list }
  | Tuple of ty list
  | Array of int * ty
  | Slice of ty

and fn_arg = FnArgument of { key : string option; mode : mode; value : expr }

and message =
  (* recv path [: tail ] *)
  | MemberMessage of { name : path; tail : expr option }
  | FnMessage of { name : path; args : fn_arg list; tail : expr option }
  | SubMessage of { name : path; args : fn_arg list; tail : expr option }
  | OpMessage of { name : string; arg : expr }

and pattern = PatNamed of path
and case = Case of { pat : pattern; body : expr }

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
  (* callee [args...] *)
  | SubCall of { callee : expr; args : fn_arg list }
  (* expr message* *)
  | Send of { recv : expr; msg : message }
  | If of { condition : expr; taken : expr; not_taken : expr }
  | When of { condition : expr; taken : expr }
  | Set of { lval : expr; operator : string option; rval : expr }
  | While of { condition : expr; body : expr }
  | Unsafe of expr
  | Yield of mode * expr
  | WhileLet of { name : string; ty : ty option; value : expr; body : expr }
  | ArrayLit of expr list
  | Cast of expr * ty
  | String of string
  | Match of { scrutinee : expr; cases : case list }

type fn_parameter =
  | FnParameter of {
      mode : mode;
      name : string;
      key : string option;
      ty : ty;
      default : expr option;
    }

type import_path =
  | ImportAtom of string
  | ImportMember of string * import_path
  | ImportMultiple of import_path list

type toplevel =
  | FunctionDefinition of {
      yields : yields;
      name : string;
      parameters : fn_parameter list;
      ty : ty option;
      value : expr option;
    }
  | SubDefinition of {
      yields : yields;
      mode : mode;
      name : string;
      parameters : fn_parameter list;
      ty : ty option;
      value : expr option;
    }
  | ConstantDefinition of { name : string; ty : ty; value : expr }
  | TypeDefinition of { name : string; ty : ty }
  | Extern of {
      abi : string option;
      name : string;
      parameters : fn_parameter list;
      ty : ty option;
    }
  | ToplevelWith of { imports : bool; path : import_path }
  | Import of import_path
