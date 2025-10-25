type mutability = Immutable | Mutable
type sharing = Data | Reference
type projection = Value | Projection
type t = Mode of projection * mutability * sharing

exception ProjectionFailure of t * t

let create ?(project : projection = Value) ?(mut : mutability = Mutable)
    ?(share : sharing = Data) () =
  (* references are always projections *)
  let project = if share == Reference then Projection else project in
  Mode (project, mut, share)

let is_mutable (Mode (_, m, _)) = m == Mutable
let is_reference (Mode (_, _, s)) = s == Reference
let is_projection (Mode (p, _, _)) = p == Projection
let implies a c = if a then c else true

(* a "compatibility" between modes *)
let ( <: ) parameter argument =
  (* both modes are either values or references at the same time *)
  let equal_sharing = is_reference parameter == is_reference argument in

  (* we only care about projections if we need a value, so *)
  let checked_projection =
    implies (not @@ is_projection parameter) (not @@ is_projection argument)
  in

  (* immutable modes are a subtype of mutable modes, but not vice versa *)
  let par_mut_implies_arg_mut =
    implies (is_mutable parameter) (is_mutable argument)
  in

  (* we only care about parameter mutability if both are references *)
  checked_projection && equal_sharing
  && implies (is_reference parameter) par_mut_implies_arg_mut

let ( >: ) = Util.Combinator.flip ( <: )

let try_project target source =
  let subtype = target <: source in
  let project_down = is_projection target && (not @@ is_projection source) in

  let implied_mut = implies (is_mutable target) (is_mutable source) in

  let can_refer =
    implies (not @@ is_reference target) (not @@ is_reference source)
  in

  let can_project = subtype || (project_down && implied_mut && can_refer) in

  if can_project then
    let (Mode (_, m, s)) = target in
    Some (Mode (Projection, m, s))
  else None

let project target source =
  match try_project target source with
  | Some m -> m
  | None -> raise (ProjectionFailure (target, source))

let unproject (Mode (_, m, s)) = Mode (Value, m, s)
let equal l r = l == r

let pp fmt (Mode (p, m, s)) =
  Format.pp_print_string fmt "Mode(";
  (match p with
  | Value -> Format.pp_print_string fmt "value"
  | Projection -> Format.pp_print_string fmt "projection");
  Format.pp_print_string fmt ", ";
  (match m with
  | Immutable -> Format.pp_print_string fmt "immutable"
  | Mutable -> Format.pp_print_string fmt "mutable");
  Format.pp_print_string fmt ", ";
  (match s with
  | Data -> Format.pp_print_string fmt "value"
  | Reference -> Format.pp_print_string fmt "reference");
  Format.pp_print_string fmt ")"

let fmt_mutability f m =
  let m = match m with Immutable -> "immutable" | Mutable -> "mutable" in
  Printf.fprintf f "%s" m

let fmt_sharing f (s : sharing) =
  let s = match s with Data -> "value" | Reference -> "reference" in
  Printf.fprintf f "%s" s

let fmt_projection f (p : projection) =
  let p = match p with Value -> "value" | Projection -> "projection" in
  Printf.fprintf f "%s" p

let fmt c (Mode (p, m, s)) =
  Printf.fprintf c "Mode(%a, %a, %a)" fmt_projection p fmt_mutability m
    fmt_sharing s
