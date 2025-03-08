type t

val create : unit -> t
val get_start : t -> Node.t
val get_stop : t -> Node.t
val set_stop_ctrl : t -> Node.t -> unit

val add_dependencies : t -> Node.t -> Node.t option list -> unit
(** Add dependencies of a node to the graph, the node doesn't need to have been added to the graph
    already.

    If the node already had dependencies, the new ones are put at the start *)
val set_dependency : t -> Node.t -> Node.t option -> int -> unit
val remove_dependency : t -> node:Node.t -> dep:Node.t -> unit
val replace_node_with: t -> Node.t -> Node.t -> unit
val iter : t -> f:(Node.t -> unit) -> unit
val get_dependencies : t -> Node.t -> Node.t option list
val get_dependency : t -> Node.t -> int -> Node.t option
val get_dependants : t -> Node.t -> Node.t list
val remove_node : t -> Node.t -> unit
val get_num_nodes : t -> int
val finalize_node: t -> Node.t -> Node.t
