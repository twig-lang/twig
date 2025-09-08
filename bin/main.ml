let subcommandp = ref true
let subcommands = [ ("check", []) ] |> List.to_seq |> Hashtbl.of_seq
let specs = ref []
let files = ref []

let anon s =
  if !subcommandp then (
    subcommandp := false;
    try specs := Hashtbl.find subcommands s
    with Not_found -> failwith ("unknown command: " ^ s))
  else files := s :: !files

let process_file path =
  let input = In_channel.open_text path in
  let raw_ast = Option.get @@ Parse.parse_chan ~fname:path input in
  let ty_ast = Typed.from_ast raw_ast in
  ignore ty_ast;
  ()

let () =
  Arg.parse_dynamic specs anon "";
  List.iter process_file !files
