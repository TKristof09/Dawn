val create : Graph.t -> Node.t * Node.t -> Node.t

val create_loop : Graph.t -> Node.t -> Node.t
(** Loop regions have two inputs, the first is the back edge the second is the head. The back edge
    will get filled later on. *)

val set_loop_back_edge : Graph.t -> Node.t -> Node.t -> unit
