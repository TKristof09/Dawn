val create_data :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, 'b) Node2.t ->
  int ->
  (Node2.any Node2.unary, Node2.data) Node2.t

val create_ctrl :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, 'b) Node2.t ->
  int ->
  (Node2.any Node2.unary, Node2.ctrl) Node2.t

val compute_type :
  Node2.G.readonly Node2.G.t ->
  ('a Node2.unary, 'b) Node2.t ->
  (new_type:Types.t * extra_deps:Node2.any list)
