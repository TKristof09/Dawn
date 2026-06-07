val create_data :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  int ->
  (Node2.any_data Node2.unary, Node2.data) Node2.t

val create_ctrl :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.ctrl) Node2.t ->
  int ->
  (Node2.any_ctrl Node2.unary, Node2.ctrl) Node2.t

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
