val create :
  ?parent_fun:int ->
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  ctrl:Node.t ->
  pred:Node.t ->
  Node.t

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
