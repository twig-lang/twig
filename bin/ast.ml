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

and argument = Argument of string option * mode * expr
(* key, mode, value *)

and message =
  | MsgMember of path * (mode * expr) option (* recv path [: tail] *)
  | MsgFn of path * argument list * (mode * expr) option
    (* recv path(...) [: tail] *)
  | MsgSub of path * argument list * (mode * expr) option
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

and module_argument =
  | ModArgModule of { name : string; ty : sigexpr option }
  | ModArgTy of path

and expr =
  (* literals and variables *)
  | ExprVariable of path
  | ExprInteger of int
  | ExprReal of float
  | ExprChar of Uchar.t
  | ExprUnit
  | ExprBool of bool
  | ExprString of string
  | ExprArray of expr list
  (* method calls *)
  | ExprTailArg of expr * expr
  | ExprCall of expr * argument list
  | ExprSubCall of expr * argument list
  | ExprSend of expr * message
  (* control flow *)
  | ExprUnsafe of expr
  | ExprBlock of expr list
  | ExprIf of expr * expr * expr
  | ExprWhen of expr * expr
  | ExprWhile of expr * expr
  | ExprMatch of expr * case list
  | ExprLabel of string * ty option * expr
  | ExprLoop of expr
  (* if/while/when let *)
  | ExprIfLet of pattern * ty option * expr * expr * expr
  | ExprWhileLet of pattern * ty option * expr * expr
  | ExprWhenLet of pattern * ty option * expr * expr
  (* if/while/when match *)
  | ExprIfMatch of pattern * ty option * expr * expr * expr
  | ExprWhileMatch of pattern * ty option * expr * expr
  | ExprWhenMatch of pattern * ty option * expr * expr
  (* statements *)
  | ExprLet of pattern * ty option * mode * expr
  | ExprSet of expr * expr
  | ExprContinue
  | ExprBreak of string option * expr option
  | ExprReturn of expr option
  | ExprYield of mode * expr
  (* others *)
  | ExprCast of expr * ty
  | ExprAddressof of ptr_mut * expr
  | ExprDeref of mode * expr
  | ExprUpdate of expr * (string * mode * expr) list
  | ExprTop of toplevel

and parameter =
  | Parameter of mode * string * ty
  | ParameterLabel of string * ty
  | ParameterKey of mode * string * expr option * ty

and toplevel =
  | TopFnDefinition of {
      unsafep : bool;
      name : fn_name;
      pos_parameters : parameter list;
      key_parameters : parameter list;
      ty : ty option;
      value : expr option;
    }
  | TopSubDefinition of {
      unsafep : bool;
      yields : yields;
      mode : mode;
      name : fn_name;
      pos_parameters : parameter list;
      key_parameters : parameter list;
      ty : ty option;
      value : expr option;
    }
  | TopConstDefinition of { name : string; ty : ty; value : expr }
  | TopTypeDefinition of { name : string; ty : ty }
  | TopTypeAbstract of string
  | TopExtern of {
      abi : string option;
      name : string;
      parameters : parameter list;
      ty : ty option;
    }
  | TopWith of { imports : bool; path : import_path }
  | TopImport of import_path
  | TopModDefinition of {
      name : string;
      signature : sigexpr option;
      args : module_argument list;
      value : modexpr;
    }
  | TopSigDefinition of {
      name : string;
      args : module_argument list;
      value : modexpr;
    }
