type variable = Variable of variable Ty.t option ref

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

type context = { this_module : variable Tree.m }

type 'tv env = {
  super : 'tv env option;
  context : context;
  return : 'tv Ty.t option;
  yield : 'tv Ty.t option;
  bound : ('tv Ty.t * Mode.t) Env.t;
}

let create_context this = { this_module = this }

let create_env ?return ?yield ?super context =
  let context = create_context context in
  { context; return; yield; super; bound = Env.empty }

let rec lookup env binding =
  try Env.read binding env.bound
  with e -> (
    match env.super with Some super -> lookup super binding | None -> raise e)
