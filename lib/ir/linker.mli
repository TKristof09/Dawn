type t

val create : unit -> t
val define : t -> ?name:string -> Node.t -> int
val link : t -> (Node.t, Graph.readwrite) Graph.t -> Node.t -> unit
val get_name : t -> int -> string
