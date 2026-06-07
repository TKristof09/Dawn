val create_new :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ctrl:('a, Node2.ctrl) Node2.t ->
  mem:('b, Node2.mem) Node2.t ->
  size:('c, Node2.data) Node2.t ->
  Types.t ->
  (Node2.alloc, Node2.mem) Node2.t

val create_store :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:('a, Node2.mem) Node2.t ->
  ptr:('b, Node2.mem) Node2.t ->
  string ->
  value:('c, Node2.data) Node2.t ->
  (Node2.store, Node2.mem) Node2.t

val create_load :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:('a, Node2.mem) Node2.t ->
  ptr:('b, Node2.mem) Node2.t ->
  string ->
  Types.t ->
  (Node2.load, Node2.mem) Node2.t

val create_addr_of :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  (Node2.addr_of, Node2.mem) Node2.t

val create_addr_of_field :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ?index:('b, Node2.data) Node2.t ->
  string ->
  (Node2.addr_of, Node2.mem) Node2.t

val create_deref :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  mem:('a, Node2.mem) Node2.t ->
  ('b, Node2.mem) Node2.t ->
  (Node2.deref, Node2.mem) Node2.t

val create_copy :
  ?parent_fun:int ->
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  mem:('a, Node2.mem) Node2.t ->
  src:('b, Node2.mem) Node2.t ->
  dst:('c, Node2.mem) Node2.t ->
  (Node2.copy, Node2.mem) Node2.t
