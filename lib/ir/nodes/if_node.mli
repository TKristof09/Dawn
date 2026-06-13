val create :
  ?parent_fun:int ->
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ctrl:('a, Node2.ctrl) Node2.t ->
  pred:('b, Node2.data) Node2.t ->
  (Node2.any_data Node2.unary, Node2.ctrl) Node2.t

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
