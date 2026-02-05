type t

val create : unit -> t
val define : t -> Node.t -> int
val link : t -> (Node.t, Graph.readwrite) Graph.t -> Node.t -> unit
