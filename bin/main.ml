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

let translate_to_modname p =
  let p = String.map (function '/' -> '.' | x -> x) p in
  if String.ends_with ~suffix:".tw" p then String.sub p 0 (String.length p - 3)
  else failwith ""

let process_file path =
  let modname = translate_to_modname path in
  Printf.printf "%s\n" modname;
  let input = In_channel.open_text path in
  let raw_ast = Option.get @@ Parse.parse_chan ~fname:path input in
  let ty_ast = Typed.from_ast_toplevels raw_ast in
  ignore ty_ast;
  ()

let () =
  Arg.parse_dynamic specs anon "";
  List.iter process_file !files
