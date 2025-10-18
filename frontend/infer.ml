open Util

type variable = Variable of variable Ty.t option ref

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

type context = {
  this_module : variable Tree.m;
  variables : variable Ty.t Disjoint_set.t;
}

type env = {
  super : env option;
  context : context;
  return : Tree.ty option;
  yield : Tree.ty option;
  bound : (Disjoint_set.set * Mode.t) Env.t;
}

let create_context this =
  { this_module = this; variables = Disjoint_set.create () }

let create_env ?return ?yield ?super context =
  let context = create_context context in
  { context; return; yield; super; bound = Env.empty }

let rec lookup env binding =
  try Env.read binding env.bound
  with e -> (
    match env.super with Some super -> lookup super binding | None -> raise e)

let unify'' env l r = Disjoint_set.union env.context.variables l r
