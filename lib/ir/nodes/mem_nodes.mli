val create_new :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ctrl:Node.t ->
  mem:Node.t ->
  size:Node.t ->
  Types.t ->
  Node.t

val create_store :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:Node.t ->
  ptr:Node.t ->
  offset:Node.t ->
  string ->
  value:Node.t ->
  Node.t

val create_load :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:Node.t ->
  ptr:Node.t ->
  string ->
  offset:Node.t ->
  Types.t ->
  Node.t

val create_addr_of :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> Node.t -> Node.t

val create_addr_of_field :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> Node.t -> string -> Node.t

val create_deref :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> mem:Node.t -> Node.t -> Node.t

val create_copy :
  ?parent_fun:int ->
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  mem:Node.t ->
  src:Node.t ->
  dst:Node.t ->
  Node.t
