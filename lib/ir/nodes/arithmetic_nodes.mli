val create_add :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_sub :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_mul :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_div :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val compute_type :
  Node.G.readonly Node.G.t ->
  (Node.binop, Node.data) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)
