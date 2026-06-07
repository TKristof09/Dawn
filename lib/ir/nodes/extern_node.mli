val create :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  Types.t ->
  string ->
  (unit, Node2.data) Node2.t
