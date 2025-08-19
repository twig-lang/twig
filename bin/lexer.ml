type t = { input : string; here : Position.t; head : int }

let ( <@@> ) f g = fun x -> f @@ g x
let ( let> ) = Option.bind

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
  let dec = String.get_utf_8_uchar s.input s.head in
  let chr = Uchar.utf_decode_uchar dec in
  Some chr

let next_char s =
  let dec = String.get_utf_8_uchar s.input s.head in
  let len = Uchar.utf_decode_length dec in
  let chr = Uchar.utf_decode_uchar dec in
  Some ({ s with head = s.head + len }, chr)

let skip_ws s =
  let here = s.here in
  let rec skip' s =
    let> h = peek_char s in
    if isws h then
      let> s, _ = next_char s in
      skip' s
    else
      Some (s, (Token.Whitespace, None, Position.set_to here s.here.byte_from))
  in
  skip' s
