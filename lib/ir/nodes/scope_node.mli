val create : unit -> Node.t
val define : Graph.t -> Node.t -> string -> Node.t -> unit
val assign : Graph.t -> Node.t -> string -> Node.t -> unit
val get : Node.t -> string -> Node.t
val push : Node.t -> unit
val pop : Graph.t -> Node.t -> unit
val dup : Graph.t -> Node.t -> Node.t

val merge : Graph.t -> this:Node.t -> other:Node.t -> unit
(** Merge the symbols from other into this, creating phi nodes if necessary *)

val get_ctrl : Node.t -> Node.t
val set_ctrl : Graph.t -> Node.t -> Node.t -> unit
