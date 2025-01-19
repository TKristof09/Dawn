type 'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val singleton : 'a -> 'a t
val empty : unit -> 'a t
val append : 'a t -> 'a t -> 'a t
val push_back : 'a -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val ( @ ) : 'a t -> 'a t -> 'a t
