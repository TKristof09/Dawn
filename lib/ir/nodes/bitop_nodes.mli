val create_lsh :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_rsh :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_band :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val create_bor :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.data) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.binop, Node2.data) Node2.t

val compute_type :
  Node2.G.readonly Node2.G.t ->
  (Node2.binop, Node2.data) Node2.t ->
  (new_type:Types.t * extra_deps:Node2.any list)
