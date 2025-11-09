type 'a t

exception Not_found of string

val empty : 'a t
val of_list : (string * 'a) list -> 'a t
val of_seq : (string * 'a) Seq.t -> 'a t
val to_list : 'a t -> (string * 'a) list
val to_seq : 'a t -> (string * 'a) Seq.t
val create : string -> 'a -> 'a t -> 'a t
val read_opt : string -> 'a t -> 'a option
val read : string -> 'a t -> 'a
val update : string -> 'a -> 'a t -> 'a t
val delete : string -> 'a t -> 'a t
val iter : (string -> 'a -> unit) -> 'a t -> unit
