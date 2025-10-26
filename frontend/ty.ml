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
  | Char
  | Str
(* Built-in, base types *)

let is_primitive_integer = function
  | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 -> true
  | _ -> false

let is_primitive_signed = function
  | I8 | I16 | I32 | I64 | F32 | F64 -> true
  | _ -> false

let is_primitive_real = function F32 | F64 -> true | _ -> false
let equal_prim l r = l == r
(*
  match (l, r) with
  | Unit, Unit
  | Bool, Bool
  | U8, U8
  | U16, U16
  | U32, U32
  | U64, U64
  | I8, I8
  | I16, I16
  | I32, I32
  | I64, I64
  | F32, F32
  | F64, F64 ->
      true
  | _ -> false*)

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

let rec equal_ext ~resolve l r =
  match (l, r) with
  | Enumeration el, Enumeration er ->
      List.for_all2
        (fun (ln, lt) (rn, rt) ->
          String.equal ln rn && List.for_all2 (equal ~resolve) lt rt)
        el er
  | Structure sl, Structure sr ->
      List.for_all2
        (fun (ln, lt) (rn, rt) -> String.equal ln rn && equal ~resolve lt rt)
        sl sr
  | Union ul, Union ur ->
      List.for_all2
        (fun (ln, lt) (rn, rt) -> String.equal ln rn && equal ~resolve lt rt)
        ul ur
  | _ -> false

and equal ~resolve l r =
  match (l, r) with
  | Primitive l, Primitive r -> equal_prim l r
  | Integer, Integer -> true
  | Real, Real -> true
  | Bottom, Bottom -> true
  | Named l, Named r -> Path.equal l r
  | Pointer (ml, l), Pointer (mr, r) -> ml == mr && equal ~resolve l r
  | Array (ll, l), Array (lr, r) -> ll == lr && equal ~resolve l r
  | Slice l, Slice r -> equal ~resolve l r
  | Tuple l, Tuple r -> List.for_all2 (equal ~resolve) l r
  | Extension el, Extension er -> equal_ext ~resolve el er
  | Variable l, r -> equal ~resolve (resolve l) r
  | l, Variable r -> equal ~resolve l (resolve r)
  | _ -> false

let rec map_tv map =
  let match_tv_ext map = function
    | Structure ms -> Structure (List.map (fun (n, t) -> (n, map_tv map t)) ms)
    | Union ms -> Union (List.map (fun (n, t) -> (n, map_tv map t)) ms)
    | Enumeration ms ->
        Enumeration (List.map (fun (n, ts) -> (n, List.map (map_tv map) ts)) ms)
  in
  function
  | Primitive p -> Primitive p
  | Variable v -> map v
  | Integer -> Integer
  | Real -> Real
  | Bottom -> Bottom
  | Named p -> Named p
  | Pointer (m, t) -> Pointer (m, map_tv map t)
  | Array (l, t) -> Array (l, map_tv map t)
  | Slice t -> Slice (map_tv map t)
  | Tuple ts -> Tuple (List.map (map_tv map) ts)
  | Extension e -> Extension (match_tv_ext map e)

let rec fmt ?(tv = fun _ -> "<>") f =
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
        | F64 -> "f64"
        | Char -> "char"
        | Str -> "str")
  | Real -> fprintf f "{real}"
  | Integer -> fprintf f "{integer}"
  | Bottom -> fprintf f "!"
  | Named name -> fprintf f "{named:%a}" Path.fmt name
  | Pointer (m, p) -> fprintf f "* %a %a" Mode.fmt_mutability m (fmt ~tv) p
  | Array (n, i) -> fprintf f "[%d] %a" n (fmt ~tv) i
  | Slice i -> fprintf f "[] %a" (fmt ~tv) i
  | Variable v -> fprintf f "{variable:%s}" (tv v)
  | Tuple is ->
      let rec f' c = function
        | x :: [] -> fprintf c "%a" (fmt ~tv) x
        | x :: xs ->
            fprintf c "%a, " (fmt ~tv) x;
            f' c xs
        | [] -> ()
      in
      fprintf f "(%a)" f' is
  | Extension _e -> fprintf f "<extension>"
