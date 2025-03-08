val create : Graph.t -> Node.t -> Node.t
(** Loop regions have two inputs, the first is the back edge the second is the head. The back edge
    will get filled later on. *)

val set_back_edge : Graph.t -> Node.t -> Node.t -> unit
val get_back_edge : Graph.t -> Node.t -> Node.t
val get_entry_edge : Graph.t -> Node.t -> Node.t
