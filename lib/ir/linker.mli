type t

val create : unit -> t
val define : t -> (Node2.fun_def, Node2.ctrl) Node2.t -> int
val set_name : t -> int -> string -> unit

val link :
  t ->
  Node2.G.readwrite Node2.G.t ->
  (Node2.fun_call, Node2.ctrl) Node2.t ->
  (Node2.fun_def, Node2.ctrl) Node2.t list

val get_name : t -> int -> string
