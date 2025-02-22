type t

val create : unit -> t
val get_start : t -> Node.t

val add_dependencies : t -> Node.t -> Node.t list -> unit
(** Add dependencies of a node to the graph, the node doesn't need to have been added to the graph
    already *)

val remove_dependency : t -> node:Node.t -> dep:Node.t -> unit
val iter : t -> f:(Node.t -> unit) -> unit
val get_dependencies : t -> Node.t -> Node.t list
val get_dependants : t -> Node.t -> Node.t list
val remove_node : t -> Node.t -> unit
