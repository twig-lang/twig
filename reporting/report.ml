type kind = Note | Warning | Error

let pp_kind f =
  let open Util.Combinator in
  Format.pp_print_string f <@@> function
  | Note -> "NOTE"
  | Warning -> "WARNING"
  | Error -> "ERROR"

type mark = Mark of { span : Location.span; text : string }

type t =
  | Report of { path : string; kind : kind; text : string; marks : mark list }

let present (Report r) =
  let errored = ref false in
  Format.printf "[%s ?:?] %a: %s" r.path pp_kind r.kind r.text;

  (* TODO: display r.marks *)
  if r.kind == Error then errored := true;

  !errored

let reports = ref []
let add (r : t) = reports := r :: !reports
