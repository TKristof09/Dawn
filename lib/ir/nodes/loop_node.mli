val create :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.ctrl) Node.t ->
  (Node.loop, Node.ctrl) Node.t
(** Loop regions have two inputs, the first is the back edge the second is the head. The back edge
    will get filled later on. *)

val set_back_edge :
  Node.G.readwrite Node.G.t -> (Node.loop, Node.ctrl) Node.t -> ('a, Node.ctrl) Node.t -> unit
