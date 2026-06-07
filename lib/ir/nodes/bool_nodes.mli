val create_eq :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_neq :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_lt :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_leq :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_gt :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_geq :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
