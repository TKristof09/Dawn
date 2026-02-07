val create_new :
  (Node.t, Graph.readwrite) Graph.t ->
  ctrl:Node.t ->
  mem:Node.t ->
  count:Node.t ->
  Types.node_type ->
  Node.t

val create_store :
  (Node.t, Graph.readwrite) Graph.t ->
  mem:Node.t ->
  ptr:Node.t ->
  offset:Node.t ->
  value:Node.t ->
  Node.t

val create_load :
  (Node.t, Graph.readwrite) Graph.t -> mem:Node.t -> ptr:Node.t -> offset:Node.t -> Node.t
