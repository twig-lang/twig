type 'tv positional_parameter =
  | Positional_value of Mode.t * string * 'tv Ty.t
  | Positional_label of string * 'tv Ty.t

type 'tv named_parameter =
  | Named_value of Mode.t * 'tv Ty.t
  | Named_label of 'tv Ty.t
  | Named_key of Mode.t * 'tv Ty.t * 'tv t

and 'tv positional_argument =
  | Argument_value of Mode.t * 'tv t
  | Argument_label of string

and 'tv named_argument =
  | Argument_named_value of string * Mode.t * 'tv t
  | Argument_named_label of string * string

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
  | CallFn of 'tv t * 'tv positional_argument list * 'tv named_argument list
  | CallSub of 'tv t * 'tv positional_argument list * 'tv named_argument list
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
