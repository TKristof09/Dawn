val create_add :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_sub :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_mul :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_div :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
