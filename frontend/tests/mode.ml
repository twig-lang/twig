open Frontend

module Mode_testable : Alcotest.TESTABLE = struct
  type t = Mode.t

  let equal = Mode.equal
  let pp = Mode.pp
end

(* modes are used a lot, so this avoids too much copy-paste *)
let mode ?(m : unit option) ?(s : unit option) () =
  let m =
    Option.value ~default:Mode.Immutable @@ Option.map (fun _ -> Mode.Mutable) m
  in
  let s =
    Option.value ~default:Mode.Value @@ Option.map (fun _ -> Mode.Reference) s
  in

  Mode.Mode (m, s)

let m = ()
let s = ()

let t_value_subtyping () =
  let open Mode in
  Alcotest.(check bool)
    "immutable value parameter, immutable value argument"
    (mode () <: mode ())
    true;

  Alcotest.(check bool)
    "immutable value parameter, mutable value argument"
    (mode () <: mode ~m ())
    true;

  Alcotest.(check bool)
    "mutable value parameter, immutable value argument"
    (mode ~m () <: mode ())
    true;

  Alcotest.(check bool)
    "mutable value parameter, mutable value argument"
    (mode ~m () <: mode ~m ())
    true;
  ()

let t_ref_subtyping () =
  let open Mode in
  Alcotest.(check bool)
    "immutable reference parameter, immutable reference argument"
    (mode ~s () <: mode ~s ())
    true;

  Alcotest.(check bool)
    "immutable reference parameter, mutable reference argument"
    (mode ~s () <: mode ~m ~s ())
    true;

  Alcotest.(check bool)
    "mutable reference parameter, immutable reference argument"
    (mode ~m ~s () <: mode ~s ())
    false;

  Alcotest.(check bool)
    "mutable reference parameter, mutable reference argument"
    (mode ~m ~s () <: mode ~m ~s ())
    true;
  ()

let subtyping_suite =
  [
    ("subtyping of value modes", `Quick, t_value_subtyping);
    ("subtyping of reference modes", `Quick, t_ref_subtyping);
  ]

let t_rvv () =
  let open Mode in
  Alcotest.(check bool)
    "immutable reference parameter, immutable value argument"
    (mode ~s () <: mode ())
    false;
  Alcotest.(check bool)
    "immutable reference parameter, mutable value argument"
    (mode ~s () <: mode ~m ())
    false;

  Alcotest.(check bool)
    "mutable reference parameter, immutable value argument"
    (mode ~m ~s () <: mode ())
    false;
  Alcotest.(check bool)
    "mutable reference parameter, mutable value argument"
    (mode ~m ~s () <: mode ~m ())
    false;
  ()

let t_vvr () =
  let open Mode in
  Alcotest.(check bool)
    "immutable value parameter, immutable reference argument"
    (mode () <: mode ~s ())
    false;
  Alcotest.(check bool)
    "immutable value parameter, mutable reference argument"
    (mode () <: mode ~m ~s ())
    false;

  Alcotest.(check bool)
    "mutable value parameter, immutable reference argument"
    (mode ~m () <: mode ~s ())
    false;
  Alcotest.(check bool)
    "mutable value parameter, mutable reference argument"
    (mode ~m () <: mode ~m ~s ())
    false;
  ()

let compatibility_suite =
  [
    ("incompatible reference vs value", `Quick, t_rvv);
    ("incompatible value vs reference", `Quick, t_vvr);
  ]

let () =
  Alcotest.run "Frontend.Mode"
    [ ("subtyping", subtyping_suite); ("compatibility", compatibility_suite) ]
