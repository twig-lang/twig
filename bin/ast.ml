type import_path =
  | ImportAtom of string
  | ImportMember of string * import_path
  | ImportMultiple of import_path list

type mode = Mode of bool * bool
(* is it a reference , is it mutable *)

type yields = YieldNone | YieldIf | YieldWhile
type ptr_mut = PtrConst | PtrMut
type fn_name = FnNamed of string | FnOperator of string

type lambda_kind =
  | LamFunction
  | LamFunctionPointer
  | LamSubscript
  | LamSubscriptPointer

type path =
  | PathMember of path * path
  | PathAtom of string
  | PathCall of string * path list

and struct_member = StructMember of string * ty
and enum_member = EnumMember of string * ty list option
and anon_par = AnonParameter of bool * mode * ty

and ty =
  | TyNamed of path
  | TyUnit
  | TyStruct of struct_member list
  | TyUnion of struct_member list
  | TyEnum of enum_member list
  | TyTuple of ty list
  | TyArray of int * ty
  | TySlice of ty
  | TyPointer of ptr_mut * ty
  | TyLambda of lambda_kind * anon_par list * ty option

and fn_arg = FnArgument of string option * mode * expr
(* key, mode, value *)

and message =
  | MsgMember of path * (mode * expr) option (* recv path [: tail] *)
  | MsgFn of path * fn_arg list * (mode * expr) option
    (* recv path(...) [: tail] *)
  | MsgSub of path * fn_arg list * (mode * expr) option
    (* recv path[...] [: tail] *)
  | MsgOp of string * (mode * expr)
(* recv op arg *)

and pattern =
  | PatNamed of path (* path *)
  | PatSink (* _ *)
  | PatArgs of path * pattern list (* path (...) *)

and case = Case of pattern * expr
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
  (* expr : expr *)
  | TailArg of expr * expr
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
  | Label of string * ty option * expr
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
      is_label : bool;
      mode : mode;
      name : string;
      ty : ty;
      default : expr option;
    }

and toplevel =
  | FunctionDefinition of {
      unsafep : bool;
      name : fn_name;
      pos_parameters : fn_parameter list;
      key_parameters : fn_parameter list;
      ty : ty option;
      value : expr option;
    }
  | SubDefinition of {
      unsafep : bool;
      yields : yields;
      mode : mode;
      name : fn_name;
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
