val create : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t

val define :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  string ->
  ('a, 'b) Node.t ->
  bool ->
  unit

val assign :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  ?force:bool ->
  string ->
  ('a, 'b) Node.t ->
  unit

val get : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> string -> Node.any
val push : (Node.scope_kind, Node.misc) Node.t -> unit
val pop : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> unit

val dup :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  (Node.scope_kind, Node.misc) Node.t

val dup_loop :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  (Node.scope_kind, Node.misc) Node.t

val merge :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  this:(Node.scope_kind, Node.misc) Node.t ->
  other:(Node.scope_kind, Node.misc) Node.t ->
  unit

(** Merge the symbols from other into this, creating phi nodes if necessary *)

val merge_loop :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  this:(Node.scope_kind, Node.misc) Node.t ->
  body:(Node.scope_kind, Node.misc) Node.t ->
  exit:(Node.scope_kind, Node.misc) Node.t ->
  unit

val get_ctrl : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> Node.any_ctrl

val set_ctrl :
  Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> ('a, Node.ctrl) Node.t -> unit

val get_mem : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> Node.any_mem

val set_mem :
  Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> ('a, Node.mem) Node.t -> unit

val ret_identifier : string
val get_ret_ptr : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> Node.any_data

val set_ret_ptr :
  Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> ('a, Node.data) Node.t -> unit
