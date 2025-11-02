let set = Hashtbl.create 0

let add path =
  match Hashtbl.find_opt set path with
  | Some data -> data
  | None ->
      let data = In_channel.(open_text path |> input_all) in
      Hashtbl.add set path data;
      data

let add' path data =
  Hashtbl.replace set path data;
  data

let find path = Hashtbl.find set path
