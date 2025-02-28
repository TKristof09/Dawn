type 'a t [@@deriving sexp_of]

val create : unit -> 'a t
val push : 'a t -> 'a t
val pop : 'a t -> 'a t
val find_symbol : 'a t -> string -> 'a
val reassign_symbol : 'a t -> string -> 'a -> unit
val add_symbol : 'a t -> string -> 'a -> unit
val iter : 'a t -> (name:string -> symbol:'a option -> depth:int -> unit) -> unit
val iter_current_depth : 'a t -> (name:string -> symbol:'a -> unit) -> unit
val merge : 'a t -> 'a t -> (name:string -> this:'a -> other:'a -> 'a) -> ('a -> 'a -> bool) -> unit
val show : 'a t -> ('a -> string) -> string
