type 'a t

exception Not_found of string

val empty : 'a t
val of_list : (string * 'a) list -> 'a t
val of_seq : (string * 'a) Seq.t -> 'a t
val to_list : 'a t -> (string * 'a) list
val to_seq : 'a t -> (string * 'a) Seq.t
val add : string -> 'a -> 'a t -> 'a t
val find_opt : string -> 'a t -> 'a option
val find : string -> 'a t -> 'a
val update : string -> 'a -> 'a t -> 'a t
val remove : string -> 'a t -> 'a t
val iter : (string -> 'a -> unit) -> 'a t -> unit
val filter : (string -> 'a -> bool) -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (string -> 'a -> 'b) -> 'a t -> 'b t
val fold : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
