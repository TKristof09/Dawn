val create_new :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  ctrl:Node.t ->
  mem:Node.t ->
  size:Node.t ->
  Types.t ->
  Node.t

val create_store :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  mem:Node.t ->
  ptr:Node.t ->
  offset:Node.t ->
  value:Node.t ->
  Node.t

val create_load :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  mem:Node.t ->
  ptr:Node.t ->
  string ->
  offset:Node.t ->
  Node.t
