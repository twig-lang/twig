(* lower from AST to IR in one go? *)

type inference_variable =
  | Known of Ty.t (* solved variable *)
  | Unknown (* unresolved type variable *)
  | Integer (* type of int literals *)
  | Real (* type of float literals *)

let parlist_of_parmap (map : Expr.parameter_map) =
  List.map (fun (p : Expr.positional) -> (p.name, p.ty)) map.positional

(* TODO: Inference. *)
let lower_value (_fnb : Ir_builder.func_builder) (bb : Ir_builder.block_builder)
    (v : Expr.t) =
  match v with
  | Expr.Unit -> Ir_builder.kunit bb
  | Expr.Bool k -> Ir_builder.kbool k bb
  | _ -> failwith "TODO"

let lower_fn_value (fnb : Ir_builder.func_builder) (v : Expr.t) =
  let bb = fnb.build_block () in

  let tval = lower_value fnb bb v in
  Ir_builder.return tval bb;

  fnb.add_block (bb.get ())

let lower_fn_def (def : Tree.fn_definition) =
  let s = def.signature in
  let params = parlist_of_parmap s.parameters in
  let ret = s.return in
  let builder = Ir_builder.create_func_builder params ret () in

  let entry = lower_fn_value builder def.value in

  builder.set_entry entry;
  builder.get ()
