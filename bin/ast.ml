type import_path =
  | ImportAtom of string
  | ImportMember of string * import_path
  | ImportMultiple of import_path list

type mode = Mode of { is_ref : bool; is_mut : bool }
type yields = Returns | YieldIf | YieldWhile
type ptr_mut = PtrConst | PtrMut

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
  | Pointer of ptr_mut * ty

and fn_arg = FnArgument of { key : string option; mode : mode; value : expr }

and message =
  (* recv path [: tail ] *)
  | MemberMessage of { name : path; tail : (mode * expr) option }
  | FnMessage of {
      name : path;
      args : fn_arg list;
      tail : (mode * expr) option;
    }
  | SubMessage of {
      name : path;
      args : fn_arg list;
      tail : (mode * expr) option;
    }
  | OpMessage of { name : string; arg : mode * expr }

and pattern = PatNamed of path | PatSink | PatArgs of path * pattern list
and case = Case of { pat : pattern; body : expr }
and modexpr = ModBody of toplevel list | ModPath of path
and sigexpr = SigNamed of path | SigJoin of path list

and mod_arg =
  | ModArgModule of { name : string; ty : sigexpr option }
  | ModArgTy of path

and expr =
  | Variable of path
  | Integer of int
  | Real of float
  | Char of Uchar.t
  | Unit
  | Bool of bool
  | Block of expr list
  (* NOTE: typed as () *)
  | Let of { bind : pattern; ty : ty option; mode : mode; value : expr }
  (* callee (args...) *)
  | FnCall of { callee : expr; args : fn_arg list }
  (* callee [args...] *)
  | SubCall of { callee : expr; args : fn_arg list }
  (* expr message* *)
  | Send of { recv : expr; msg : message }
  | If of { condition : expr; taken : expr; not_taken : expr }
  | When of { condition : expr; taken : expr }
  | Set of { lval : expr; rval : expr }
  | While of { condition : expr; body : expr }
  | Unsafe of expr
  | Yield of mode * expr
  | ArrayLit of expr list
  | Cast of expr * ty
  | String of string
  | Match of { scrutinee : expr; cases : case list }
  | Addressof of ptr_mut * expr
  | Deref of mode * expr
  | Top of toplevel
  | Update of expr * (string * mode * expr) list
  | Label of string * expr
  | Continue
  | Break of string option * expr option
  | Return of expr option
  | Loop of expr
  | IfLet of {
      bind : pattern;
      ty : ty option;
      value : expr;
      taken : expr;
      not_taken : expr;
    }
  | WhileLet of { bind : pattern; ty : ty option; value : expr; body : expr }
  | WhenLet of { bind : pattern; ty : ty option; value : expr; body : expr }
  | IfMatch of {
      bind : pattern;
      ty : ty option;
      value : expr;
      taken : expr;
      not_taken : expr;
    }
  | WhileMatch of { bind : pattern; ty : ty option; value : expr; body : expr }
  | WhenMatch of { bind : pattern; ty : ty option; value : expr; body : expr }

and fn_parameter =
  | FnParameter of {
      mode : mode;
      name : string;
      ty : ty;
      default : expr option;
    }

and toplevel =
  | FunctionDefinition of {
      unsafep : bool;
      name : string;
      pos_parameters : fn_parameter list;
      key_parameters : fn_parameter list;
      ty : ty option;
      value : expr option;
    }
  | SubDefinition of {
      unsafep : bool;
      yields : yields;
      mode : mode;
      name : string;
      pos_parameters : fn_parameter list;
      key_parameters : fn_parameter list;
      ty : ty option;
      value : expr option;
    }
  | ConstantDefinition of { name : string; ty : ty; value : expr }
  | TypeDefinition of { name : string; ty : ty }
  | TypeAbstract of string
  | Extern of {
      abi : string option;
      name : string;
      parameters : fn_parameter list;
      ty : ty option;
    }
  | ToplevelWith of { imports : bool; path : import_path }
  | Import of import_path
  | ModDefinition of {
      name : string;
      signature : sigexpr option;
      args : mod_arg list;
      value : modexpr;
    }
  | SigDefinition of { name : string; args : mod_arg list; value : modexpr }
