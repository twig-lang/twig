type 'tv positional_parameter =
  | PPValue of Mode.t * string * 'tv Ty.t
  | PPLabel of string * 'tv Ty.t

type 'tv named_parameter =
  | PNValue of Mode.t * string * 'tv Ty.t
  | PNLabel of string * 'tv Ty.t
  | PNKey of Mode.t * string * 'tv Ty.t * 'tv t

and 'tv positional_argument = AValue of Mode.t * 'tv t
and 'tv named_argument = ANamedValue of Mode.t * string * 'tv t

and 'tv t =
  | Unit
  | Int of int
  | Real of float
  | Bool of bool
  | String of string
  | Char of Uchar.t
  | Tuple of 'tv t list
  | List of 'tv t list
  | Variable of Path.t
  | If of 'tv t * 'tv t * 'tv t
  | Return of 'tv t
  (* returned type, non-returned values (of type ()) and returned value *)
  | Block of 'tv t list * 'tv t
  (* returned type, function, positional, named *)
  | Call of 'tv t * 'tv positional_argument list * 'tv named_argument list

(* Expressions *)

type 'tv param_list = 'tv positional_parameter list * 'tv named_parameter list
