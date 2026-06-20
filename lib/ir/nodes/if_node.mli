val create :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ctrl:('a, Node.ctrl) Node.t ->
  pred:('b, Node.data) Node.t ->
  (Node.any_data Node.unary, Node.ctrl) Node.t

val compute_type :
  Node.G.readonly Node.G.t ->
  (Node.any_data Node.unary, Node.ctrl) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)
