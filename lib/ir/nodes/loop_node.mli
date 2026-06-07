val create :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.ctrl) Node2.t ->
  (Node2.loop, Node2.ctrl) Node2.t
(** Loop regions have two inputs, the first is the back edge the second is the head. The back edge
    will get filled later on. *)

val set_back_edge :
  Node2.G.readwrite Node2.G.t ->
  (Node2.loop, Node2.ctrl) Node2.t ->
  ('a, Node2.ctrl) Node2.t ->
  unit
