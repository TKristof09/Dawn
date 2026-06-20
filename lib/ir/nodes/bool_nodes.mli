val create_eq :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_neq :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_lt :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_leq :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_gt :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.data) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.binop, Node.data) Node.t

val create_geq :
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
