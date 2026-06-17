val create : Node2.G.readwrite Node2.G.t -> (Node2.scope_kind, Node2.misc) Node2.t

val define :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  string ->
  ('a, 'b) Node2.t ->
  bool ->
  unit

val assign :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  string ->
  ('a, 'b) Node2.t ->
  unit

val get :
  Node2.G.readwrite Node2.G.t -> (Node2.scope_kind, Node2.misc) Node2.t -> string -> Node2.any

val push : (Node2.scope_kind, Node2.misc) Node2.t -> unit
val pop : Node2.G.readwrite Node2.G.t -> (Node2.scope_kind, Node2.misc) Node2.t -> unit

val dup :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  (Node2.scope_kind, Node2.misc) Node2.t

val dup_loop :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  (Node2.scope_kind, Node2.misc) Node2.t

val merge :
  ?parent_fun:int ->
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  this:(Node2.scope_kind, Node2.misc) Node2.t ->
  other:(Node2.scope_kind, Node2.misc) Node2.t ->
  unit

(** Merge the symbols from other into this, creating phi nodes if necessary *)

val merge_loop :
  ?parent_fun:int ->
  Node2.G.readwrite Node2.G.t ->
  this:(Node2.scope_kind, Node2.misc) Node2.t ->
  body:(Node2.scope_kind, Node2.misc) Node2.t ->
  exit:(Node2.scope_kind, Node2.misc) Node2.t ->
  unit

val get_ctrl :
  Node2.G.readwrite Node2.G.t -> (Node2.scope_kind, Node2.misc) Node2.t -> Node2.any_ctrl

val set_ctrl :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  ('a, Node2.ctrl) Node2.t ->
  unit

val get_mem : Node2.G.readwrite Node2.G.t -> (Node2.scope_kind, Node2.misc) Node2.t -> Node2.any_mem

val set_mem :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  ('a, Node2.mem) Node2.t ->
  unit

val ret_identifier : string

val get_ret_ptr :
  Node2.G.readwrite Node2.G.t -> (Node2.scope_kind, Node2.misc) Node2.t -> Node2.any_data

val set_ret_ptr :
  Node2.G.readwrite Node2.G.t ->
  (Node2.scope_kind, Node2.misc) Node2.t ->
  ('a, Node2.data) Node2.t ->
  unit
