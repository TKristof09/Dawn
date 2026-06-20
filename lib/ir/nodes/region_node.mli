val create :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  Node.any_ctrl list ->
  (Node.merge_point, Node.ctrl) Node.t

val compute_type :
  Node.G.readonly Node.G.t ->
  (Node.merge_point, Node.ctrl) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)
