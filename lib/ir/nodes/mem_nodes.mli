val create_new :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ctrl:('a, Node.ctrl) Node.t ->
  mem:('b, Node.mem) Node.t ->
  size:('c, Node.data) Node.t ->
  Types.t ->
  (Node.alloc, Node.mem) Node.t

val create_store :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:('a, Node.mem) Node.t ->
  ptr:('b, Node.data) Node.t ->
  string ->
  value:('c, Node.data) Node.t ->
  (Node.store, Node.mem) Node.t

val create_load :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:('a, Node.mem) Node.t ->
  ptr:('b, Node.data) Node.t ->
  string ->
  Types.t ->
  (Node.load, Node.data) Node.t

val create_addr_of :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  (Node.addr_of, Node.data) Node.t

val create_addr_of_field :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ?index:('b, Node.data) Node.t ->
  string ->
  (Node.addr_of, Node.data) Node.t

val create_deref :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:('a, Node.mem) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.deref, Node.data) Node.t

val create_copy :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  mem:('a, Node.mem) Node.t ->
  src:('b, Node.data) Node.t ->
  dst:('c, Node.data) Node.t ->
  (Node.copy, Node.mem) Node.t
