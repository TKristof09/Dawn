val create :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  Types.t ->
  string ->
  (Node2.any_data Node2.phi, Node2.data) Node2.t list ->
  (Node2.extern_fun, Node2.data) Node2.t
