type set
type 'a t

val create : unit -> 'a t
val add : 'a t -> 'a -> set
val find : 'a t -> set -> set
val union : 'a t -> set -> set -> unit
