type t = { input : string; here : Position.t; head : int }

let ( <@@> ) f g = fun x -> f @@ g x
let ( let> ) = Option.bind
let ( <|> ) l r = fun s -> match l s with None -> r s | x -> x

let keywords =
  let open Token in
  [
    ("break", KBreak);
    ("const", KConst);
    ("continue", KContinue);
    ("else", KElse);
    ("enum", KEnum);
    ("fn", KFn);
    ("if", KIf);
    ("let", KLet);
    ("loop", KLoop);
    ("match", KMatch);
    ("mod", KMod);
    ("mut", KMut);
    ("return", KReturn);
    ("set", KSet);
    ("struct", KStruct);
    ("sub", KSub);
    ("type", KType);
    ("union", KUnion);
    ("unsafe", KUnsafe);
    ("where", KWhere);
    ("while", KWhile);
    ("with", KWith);
    ("yield", KYield);
  ]
  |> List.to_seq |> Hashtbl.of_seq

let create input = { input; here = Position.create (); head = 0 }

let isws =
  ( function ' ' | '\r' | '\n' -> true | _ -> false ) <@@> Uchar.to_char

let peek_char s =
  let len = String.length s.input in
  if s.head >= len then None
  else
    let dec = String.get_utf_8_uchar s.input s.head in
    let chr = Uchar.utf_decode_uchar dec in
    Some chr

let next_char s =
  let> chr = peek_char s in
  Some ({ s with here = Position.step s.here chr }, chr)

let pass ?(data = None) ?(from = None) s tag =
  let from = Option.value ~default:s.here from in
  Some (s, (tag, data, Position.set_to from s.here.byte_from))

let skip_ws s =
  let from = s.here in
  let rec skip' s =
    let> h = peek_char s in
    if isws h then
      let> s, _ = next_char s in
      skip' s
    else pass s ~from:(Some from) Token.Whitespace
  in
  skip' s

let eof s = pass s Token.Eof

let lex s =
  let parser = skip_ws <|> eof in
  parser s
