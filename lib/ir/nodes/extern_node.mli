val create :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  Types.t ->
  string ->
  (Node.any_data Node.phi, Node.data) Node.t list ->
  (Node.extern_fun, Node.data) Node.t
