type argument =
  (* MODE VALUE *)
  | Positional of { mode : Mode.t; value : t }
  (* NAME : MODE VALUE *)
  | Named of { name : string; mode : Mode.t; value : t }
  (* label NAME : value *)
  | Label of { name : string; value : string }

and argument_map = {
  (* arguments in order of evaluation *)
  ordered : argument list;
  (* positional arguments *)
  positional : (Mode.t * t) list;
  (* named arguments *)
  named : (Mode.t * t) Map.t;
  (* label arguments *)
  labels : string Map.t;
}

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
  | CallFn of t * argument_map
  | CallSub of t * argument_map
  (* name, mode, declared type, value *)
  | Let of string * Mode.t * Ty.t option * t
  | While of t * t
  | Loop of t
  (* name, declared type,  body *)
  | Label of string * Ty.t * t
  (* name, value (default the unit literal) *)
  | Break of string * t
  | Yield of Mode.t * t
  | Set of t * t
  | When of t * t
  | PathMember of Path.t * t
(* Expressions *)

let empty_argument_map =
  { ordered = []; positional = []; named = Map.empty; labels = Map.empty }

let argument_map_of_list arguments =
  let work map = function
    | Positional { mode; value } ->
        let positional = (mode, value) :: map.positional in
        { map with positional }
    | Named { name; mode; value } ->
        let named = Map.add name (mode, value) map.named in
        { map with named }
    | Label { name; value } ->
        let labels = Map.add name value map.labels in
        { map with labels }
  in

  let map = List.fold_left work empty_argument_map arguments in

  { map with ordered = arguments; positional = List.rev map.positional }

type located = t Reporting.Location.t
type annotated = (Mode.t * Ty.t * t) Reporting.Location.t

(* A parameter of the form { MODE NAME : TY  }. *)
type positional = { name : string; mode : Mode.t; ty : Ty.t }

(* A parameter of the form { * MODE NAME : TY }. Has to be passed by name as a
     keyword argument. *)
type named = { name : string; mode : Mode.t; ty : Ty.t }

(* A parameter of the form { @ NAME : TY = VALUE }. When a function with an
   optional parameter is called without passing this parameter, a default value
   is evaluated and passed instead. Since references cannot be "materialized",
   these parameters have no mode. *)
type optional = { name : string; ty : Ty.t; default : t }

(* A parameter of the form { label NAME : TY }. It always behaves like a named
     parameter, but for labels (note there are no positional labels) *)
type label = { name : string; ty : Ty.t }

(* A convenience type for parameters. *)
type parameter =
  | Positional of positional
  | Named of named
  | Optional of optional
  | Label of label

let parameter_name = function
  | Positional { name; _ }
  | Named { name; _ }
  | Optional { name; _ }
  | Label { name; _ } ->
      name

let parameter_ty = function
  | Positional { ty; _ }
  | Named { ty; _ }
  | Optional { ty; _ }
  | Label { ty; _ } ->
      ty

(* Rough note on "meaning":
fn pos (l: i32) -> i32 = i32;
fn named ( *l: i32) -> i32 = l; {written as {( *} because ocaml}
fn opt (l: i32 = 0) -> i32 = l;
fn label (label l: i32) -> () =
  break l with 0;

pos(0);
named(l: 0);              {arg must be passed}
opt(l: 0);    OR   opt(); {arg may not be passed}

let l = 0;
named(l);                 {elided, same as named(l: l)}
opt(l);                   {same here}

label lab do              {same as in named()}
  label(label l: lab);

label l do                {as well}
  label(label l);
*)

(* uhhh *)
type parameter_map = {
  positional : positional list;
  positional_names : positional Map.t;
  named : named Map.t;
  optionals : optional Map.t;
  labels : label Map.t;
}

let empty_parameter_map =
  {
    positional = [];
    positional_names = Map.empty;
    named = Map.empty;
    optionals = Map.empty;
    labels = Map.empty;
  }

let parameter_map_of_list parameters =
  let rec work map = function
    | x :: xs ->
        let map =
          match x with
          | Positional par ->
              let positional = par :: map.positional in
              let positional_names =
                Map.add par.name par map.positional_names
              in
              { map with positional; positional_names }
          | Named par ->
              let named = Map.add par.name par map.named in
              { map with named }
          | Optional par ->
              let optionals = Map.add par.name par map.optionals in
              { map with optionals }
          | Label par ->
              let labels = Map.add par.name par map.labels in
              { map with labels }
        in
        work map xs
    | [] -> map
  in
  work empty_parameter_map parameters

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
