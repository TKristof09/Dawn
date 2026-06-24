type t

val create : unit -> t
val define : t -> (Node.fun_def, Node.ctrl) Node.t -> int
val set_name : t -> int -> string -> unit

val link :
  t ->
  Node.G.readwrite Node.G.t ->
  (Node.fun_call, Node.ctrl) Node.t ->
  (Node.fun_def, Node.ctrl) Node.t list

val get_name : t -> int -> string
val iter_fun_nodes : t -> Types.t -> f:(int -> (Node.fun_def, Node.ctrl) Node.t -> unit) -> unit
