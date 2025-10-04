type import_path =
  | ImportAtom of string
  | ImportMember of string * import_path
  | ImportMultiple of import_path list

type mode = Mode of bool * bool
(* is it a reference , is it mutable *)

type yields = YieldNone | YieldIf | YieldWhile
type ptr_mut = PtrConst | PtrMut

type lambda_kind =
  | LamFunction
  | LamFunctionPointer
  | LamSubscript
  | LamSubscriptPointer

type path =
  | PathMember of path * string
  | PathAtom of string
  | PathCall of path * path list

and struct_member = StructMember of string * ty
and enum_member = EnumMember of string * ty list option
and anon_par = AnonParameter of bool * mode * ty (* label? , mode , type *)

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
  | TySink

and argument = Argument of mode * expr
and named_argument = NamedArgument of string * mode * expr
(* key, mode, value *)

and message =
  | MsgMember of path (* recv path *)
  | MsgFn of path * argument list * named_argument list (* recv path(...) *)
  | MsgSub of path * argument list * named_argument list (* recv path[...] *)
  | MsgOp of string * (mode * expr)
  (* recv op arg *)
  | MsgUnary of string (* op recv *)

and pattern =
  | PatNamed of path (* path *)
  | PatSink (* _ *)
  | PatArgs of path * pattern list (* path (...) *)

and case = Case of pattern * expr
and modexpr = ModBody of toplevel list | ModPath of path
and sigexpr = SigNamed of path | SigJoin of path list

and module_argument =
  | ModArgModule of string * sigexpr option
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
  | ExprLambda of
      lambda_kind * (parameter list * parameter list) * ty option * expr
  (* method calls *)
  | ExprCall of expr * argument list * named_argument list
  | ExprSubCall of expr * argument list * named_argument list
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
      name : string;
      pos_parameters : parameter list;
      key_parameters : parameter list;
      ty : ty option;
      value : expr option;
    }
  | TopSubDefinition of {
      unsafep : bool;
      yields : yields;
      mode : mode;
      name : string;
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
