type 'a t

module type GraphNode = sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val semantic_equal : t -> t option list -> t -> t option list -> bool
  val hash : t -> int
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val is_persistent : t -> bool
end

val create : (module GraphNode with type t = 'a) -> 'a -> 'a -> 'a t
val get_start : 'a t -> 'a
val get_stop : 'a t -> 'a
val set_stop_ctrl : 'a t -> 'a -> unit

val add_dependencies : 'a t -> 'a -> 'a option list -> unit
(** Add dependencies of a node to the graph, the node doesn't need to have been added to the graph
    already.

    If the node already had dependencies, the new ones are put at the end *)

val set_dependency : 'a t -> 'a -> 'a option -> int -> unit
val remove_dependency : 'a t -> node:'a -> dep:'a -> unit
val replace_node_with : 'a t -> 'a -> 'a -> unit
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val find : 'a t -> f:('a -> bool) -> 'a option
val get_dependencies : 'a t -> 'a -> 'a option list
val get_dependency : 'a t -> 'a -> int -> 'a option
val get_dependants : 'a t -> 'a -> 'a list
val remove_node : 'a t -> 'a -> unit
val get_num_nodes : 'a t -> int
val finalize_node : 'a t -> 'a -> 'a
val cleanup : 'a t -> unit
