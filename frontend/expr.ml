type positional_parameter =
  | Positional_value of Mode.t * string * Ty.t
  | Positional_label of string * Ty.t

type named_parameter =
  | Named_value of Mode.t * Ty.t
  | Named_label of Ty.t
  | Named_key of Mode.t * Ty.t * t

and positional_argument =
  | Argument_value of Mode.t * t
  | Argument_label of string

and named_argument =
  | Argument_named_value of string * Mode.t * t
  | Argument_named_label of string * string

and t =
  | Unit
  | Int of int
  | Real of float
  | Bool of bool
  | String of string
  | Char of Uchar.t
  | Tuple of t list
  | List of t list
  | Variable of string
  | If of t * t * t
  | Return of t
  (* returned type, non-returned values (of type ()) and returned value *)
  | Block of t list * t
  (* returned type, function, positional, named *)
  | CallFn of t * positional_argument list * named_argument list
  | CallSub of t * positional_argument list * named_argument list
  (* name, mode, declared type, value *)
  | Let of string * Mode.t * Ty.t option * t
  | While of t * t
  | Loop of t
  (* name, declared type,  body *)
  | Label of string option * Ty.t * t
  (* name, value (default the unit literal) *)
  | Break of string option * t
  | Yield of Mode.t * t
  | Set of t * t
  | When of t * t
  | PathMember of Path.t * t
(* Expressions *)

type param_list = positional_parameter list * named_parameter Map.t
type located = t Reporting.Location.t
type annotated = (Mode.t * Ty.t * t) Reporting.Location.t

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
