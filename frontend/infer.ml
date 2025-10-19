type variable = Variable of variable Ty.t option ref
type resolved = unit

exception TypeMismatch of variable Ty.t * variable Ty.t

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

(* this environment's "local context"*)
type 'tv env =
  | Root of {
      context : variable Tree.t;
      expect_return : 'tv Ty.t option;
      expect_yield : 'tv Ty.t option;
      bound : ('tv Ty.t * Mode.t) Env.t;
    }
  | Child of { super : 'tv env; bound : ('tv Ty.t * Mode.t) Env.t }

let create_env ?expect_return ?expect_yield context =
  Root { context; expect_return; expect_yield; bound = Env.empty }

let create_subenv ~super = Child { super; bound = Env.empty }

let rec lookup env binding =
  match env with
  | Root { bound; _ } -> Env.read binding bound
  | Child { bound; super; _ } -> (
      try Env.read binding bound with _ -> lookup super binding)

(* Create a Tree from a list of toplevel statements *)
let tree_of_toplevels tops = List.fold_left Tree.add Tree.empty tops

(*( Perform type-checking and inference )*)
let mismatch l r = raise @@ TypeMismatch (l, r)

let unify_primitive l r =
  if Ty.equal_prim l r then Ty.Primitive l
  else mismatch (Ty.Primitive l) (Ty.Primitive r)

(* "unify" two types, and return the unified version *)
let rec unify (l : variable Ty.t) (r : variable Ty.t) =
  match (l, r) with
  (* Unify two primitives iff they are equal *)
  | Ty.Primitive l, Ty.Primitive r -> unify_primitive l r
  (* Bottom unifies with any other type *)
  | Ty.Bottom, _ -> r
  | _, Ty.Bottom -> l
  (* Variables get populated if empty, unified if not *)
  | Ty.Variable l, _ -> (
      match get l with
      | Some t -> unify t r
      | None ->
          set l r;
          r)
  | _, Ty.Variable r -> (
      match get r with
      | Some t -> unify l t
      | None ->
          set r l;
          l)
  (* TODO: The rest of the cases. *)
  (* Any other case is a mismatch *)
  | _ -> mismatch l r

(* only perform the type check *)
let check l r = ignore @@ unify l r

(* infer the type of an expression *)
let infer (_env : variable env) expr : variable Ty.t =
  match expr with _ -> failwith "expression not yet supported"

(*( Resolve and remove any type variables: variable Tree.t -> resolved Tree.t )*)

(*( Apply inference and resolution to the whole module )*)
let resolve_module ~parent m =
  ignore parent;
  ignore m;
  failwith "module-wide variable elimination TODO"
