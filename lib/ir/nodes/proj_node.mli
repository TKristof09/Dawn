val create_data :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, 'b) Node.t ->
  int ->
  (Node.any Node.unary, Node.data) Node.t

val create_ctrl :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, 'b) Node.t ->
  int ->
  (Node.any Node.unary, Node.ctrl) Node.t

val create_mem :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, 'b) Node.t ->
  int ->
  (Node.any Node.unary, Node.mem) Node.t

val compute_type :
  Node.G.readonly Node.G.t ->
  (Node.any Node.unary, 'a) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)
