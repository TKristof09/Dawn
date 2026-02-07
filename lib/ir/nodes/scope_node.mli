val create : unit -> Node.t
val define : (Node.t, Graph.readwrite) Graph.t -> Node.t -> string -> Node.t -> unit
val assign : (Node.t, Graph.readwrite) Graph.t -> Node.t -> string -> Node.t -> unit
val get : (Node.t, Graph.readwrite) Graph.t -> Node.t -> string -> Node.t
val push : Node.t -> unit
val pop : (Node.t, Graph.readwrite) Graph.t -> Node.t -> unit
val dup : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t
val dup_loop : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t
val merge : (Node.t, Graph.readwrite) Graph.t -> this:Node.t -> other:Node.t -> unit

(** Merge the symbols from other into this, creating phi nodes if necessary *)

val merge_loop :
  (Node.t, Graph.readwrite) Graph.t -> this:Node.t -> body:Node.t -> exit:Node.t -> unit

val get_ctrl : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t
val set_ctrl : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t -> unit
val get_mem : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t
val set_mem : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t -> unit
