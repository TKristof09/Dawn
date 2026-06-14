val create :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  Node2.any_ctrl list ->
  (Node2.merge_point, Node2.ctrl) Node2.t

val compute_type :
  Node2.G.readonly Node2.G.t ->
  (Node2.merge_point, Node2.ctrl) Node2.t ->
  (new_type:Types.t * extra_deps:Node2.any list)
