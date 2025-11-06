module M (S : Stage.S) = struct
  type positional_parameter =
    | Parameter_value of { name : string; ty : S.ty; mode : Mode.t }
    | Parameter_label of { name : string; ty : S.ty }

  and named_parameter =
    | Parameter_value of { mode : Mode.t; ty : S.ty }
    | Parameter_label of { ty : S.ty }
    | Parameter_key of { mode : Mode.t; ty : S.ty; default : t }

  and positional_argument =
    | Argument_value of { mode : Mode.t; value : t }
    | Argument_label of { label : string }

  and named_argument =
    | Argument_value of { name : string; mode : Mode.t; value : t }
    | Argument_label of { name : string; label : string }

  and t =
    | Unit
    | Int of { value : int }
    | Real of { value : float }
    | Bool of { value : bool }
    | String of { value : string }
    | Char of { value : Uchar.t }
    (* (* TODO: Uncomment later*)
    | Tuple of { members : t list }
    | List of { items : t list }
    *)
    | Variable of { name : string }
    | If of { condition : t; on_true : t; on_false : t }
    | Return of { value : t }
    | Block of { units : t list; return : t }
    | Call_fn of {
        callee : t;
        positional_arguments : positional_argument list;
        named_arguments : named_argument list;
      }
    | Call_sub of {
        callee : t;
        positional_arguments : positional_argument list;
        named_arguments : named_argument list;
      }
    | Let of { name : string; mode : Mode.t; ty : S.ty option; value : t }
    | While of { condition : t; body : t }
    | Loop of { body : t }
    | Label of { name : string option; ty : S.ty; body : t }
    | Break of { name : string option; value : t }
    | Yield of { mode : Mode.t; value : t }
    | Set of { target : t; source : t }
    | When of { condition : t; body : t }
    | Path_member of { path : Path.t; body : t }

  type parameter_list = positional_parameter list * named_parameter Map.t
end

type 'tv positional_parameter =
  | PPValue of Mode.t * string * 'tv Ty.t
  | PPLabel of string * 'tv Ty.t

type 'tv named_parameter =
  | PNValue of Mode.t * 'tv Ty.t
  | PNLabel of 'tv Ty.t
  | PNKey of Mode.t * 'tv Ty.t * 'tv t

and 'tv positional_argument = AValue of Mode.t * 'tv t | ALabel of string

and 'tv named_argument =
  | ANamedValue of string * Mode.t * 'tv t
  | ANamedLabel of string * string

and 'tv t =
  | Unit
  | Int of int
  | Real of float
  | Bool of bool
  | String of string
  | Char of Uchar.t
  | Tuple of 'tv t list
  | List of 'tv t list
  | Variable of string
  | If of 'tv t * 'tv t * 'tv t
  | Return of 'tv t
  (* returned type, non-returned values (of type ()) and returned value *)
  | Block of 'tv t list * 'tv t
  (* returned type, function, positional, named *)
  | FnCall of 'tv t * 'tv positional_argument list * 'tv named_argument list
  | SubCall of 'tv t * 'tv positional_argument list * 'tv named_argument list
  (* name, mode, declared type, value *)
  | Let of string * Mode.t * 'tv Ty.t option * 'tv t
  | While of 'tv t * 'tv t
  | Loop of 'tv t
  (* name, declared type,  body *)
  | Label of string option * 'tv Ty.t * 'tv t
  (* name, value (default the unit literal) *)
  | Break of string option * 'tv t
  | Yield of Mode.t * 'tv t
  | Set of 'tv t * 'tv t
  | When of 'tv t * 'tv t
  | PathMember of Path.t * 'tv t
(* Expressions *)

type 'tv param_list = 'tv positional_parameter list * 'tv named_parameter Map.t
type 'tv located = 'tv t Reporting.Location.t
type 'tv annotated = (Mode.t * 'tv Ty.t) * 'tv located

let rec reduce (f : 'a -> 'a -> 'a) m init x =
  let red = reduce f m init in
  let red' a f x =
    match x with
    | Unit | Int _ | Real _ | Bool _ | String _ | Char _ | Variable _ -> init
    | Tuple is | List is -> List.fold_left f init @@ List.map red is |> f a
    | If (vc, vt, vf) -> red vc |> f a |> f (red vt) |> f (red vf)
    | Block (us, v) ->
        List.fold_left f init @@ List.map red us |> f (red v) |> f a
    | While (c, v) -> red c |> f a |> f (red v)
    | Return v | Loop v | Label (_, _, v) | Break (_, v) | Yield (_, v) ->
        f a (red v)
    | _ -> failwith "not supported yet"
  in
  red' init f x |> f (m x)
