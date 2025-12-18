val create : Node.t Graph.t -> Node.t -> Node.t
(** Loop regions have two inputs, the first is the back edge the second is the head. The back edge
    will get filled later on. *)

val set_back_edge : 'a Graph.t -> 'a -> 'a -> unit
val get_back_edge : 'a Graph.t -> 'a -> 'a
val get_entry_edge : 'a Graph.t -> 'a -> 'a
