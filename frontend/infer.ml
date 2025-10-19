type variable = Variable of variable Ty.t option ref
type resolved = unit

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

(* this environment's "global context" *)
type context = { this_module : variable Tree.t }

(* this environment's "local context"*)
type 'tv env = {
  super : 'tv env option;
  context : context;
  return : 'tv Ty.t option;
  yield : 'tv Ty.t option;
  bound : ('tv Ty.t * Mode.t) Env.t;
}

let create_context this_module = { this_module }

let create_env ?return ?yield ?super context =
  let context = create_context context in
  { context; return; yield; super; bound = Env.empty }

let rec lookup env binding =
  try Env.read binding env.bound
  with e -> (
    match env.super with Some super -> lookup super binding | None -> raise e)

(* Create a Tree from a list of toplevel statements *)
let tree_of_toplevels tops = List.fold_left Tree.add Tree.empty tops

(* Perform type-checking and inference *)

(* Resolve and remove any type variables: variable Tree.t -> resolved Tree.t *)
