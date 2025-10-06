(* always points to the element's parent *)
type set = int
type 'a t = (set ref * 'a) Dynarray.t

let create () : 'a t = Dynarray.create ()

let add set item =
  let length = Dynarray.length set in
  Dynarray.add_last set (ref length, item);
  length

let get set item = snd @@ Dynarray.get set item

let rec find set item =
  let repr item = fst (Dynarray.get set item) in

  let parent = repr item in
  if !parent <> item then (
    parent := find set !parent;
    !parent)
  else item

let union set left right =
  let repr item = fst (Dynarray.get set item) in

  let fl = find set left in
  let fr = find set right in

  repr fl := fr
