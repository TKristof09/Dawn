val create_data :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.ctrl) Node2.t ->
  Node2.any_data list ->
  (Node2.any_data Node2.phi, Node2.data) Node2.t

val create_mem :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.ctrl) Node2.t ->
  Node2.any_mem list ->
  (Node2.any_mem Node2.phi, Node2.mem) Node2.t

val create_data_no_backedge :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.ctrl) Node2.t ->
  ('b, Node2.data) Node2.t ->
  (Node2.any_data Node2.phi, Node2.data) Node2.t

val create_mem_no_backedge :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node2.ctrl) Node2.t ->
  ('b, Node2.mem) Node2.t ->
  (Node2.any_mem Node2.phi, Node2.mem) Node2.t

val add_backedge_input :
  Node2.G.readwrite Node2.G.t -> ('a Node2.phi, 'b) Node2.t -> ('c, 'b) Node2.t -> unit

val add_input : Node2.G.readwrite Node2.G.t -> ('a Node2.phi, 'b) Node2.t -> 'a -> unit

val compute_type :
  Node2.G.readonly Node2.G.t ->
  ('a Node2.phi, 'b) Node2.t ->
  (new_type:Types.t * extra_deps:Node2.any list)
