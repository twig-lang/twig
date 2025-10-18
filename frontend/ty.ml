type primitive =
  | Unit
  | Bool
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | F32
  | F64
(* Built-in, base types *)

type 'v ext =
  | Enumeration of (string * 'v t list) list
  | Structure of (string * 'v t) list
  | Union of (string * 'v t) list

and 'variable t =
  | Primitive of primitive
  | Integer
  | Real
  | Bottom
  | Named of Path.t
  | Pointer of Mode.mutability * 'variable t
  | Array of int * 'variable t
  | Slice of 'variable t
  | Tuple of 'variable t list
  (* as defined by type definitions *)
  | Extension of 'variable ext
  (* the type variable itself *)
  | Variable of 'variable
(* Types *)

let rec fmt f =
  let open Printf in
  function
  | Primitive p ->
      fprintf f
        (match p with
        | Unit -> "()"
        | Bool -> "bool"
        | U8 -> "u8"
        | U16 -> "u16"
        | U32 -> "u32"
        | U64 -> "u64"
        | I8 -> "i8"
        | I16 -> "i16"
        | I32 -> "i32"
        | I64 -> "i64"
        | F32 -> "f32"
        | F64 -> "f64")
  | Real -> fprintf f "{real}"
  | Integer -> fprintf f "{integer}"
  | Bottom -> fprintf f "!"
  | Named name -> fprintf f "%a" Path.fmt name
  | Pointer (m, p) -> fprintf f "* %a %a" Mode.fmt_mutability m fmt p
  | Array (n, i) -> fprintf f "[%d] %a" n fmt i
  | Slice i -> fprintf f "[] %a" fmt i
  | Variable _ -> fprintf f "<variable>"
  | Tuple is ->
      let rec f' c = function
        | x :: [] -> fprintf c "%a" fmt x
        | x :: xs ->
            fprintf c "%a, " fmt x;
            f' c xs
        | [] -> ()
      in
      fprintf f "(%a)" f' is
  | Extension _e -> fprintf f "<extension>"
