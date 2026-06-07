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

val get_ctrl : 'a Node2.G.t -> ('b Node2.phi, 'c) Node2.t -> ('d, Node2.ctrl) Node2.t

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

val add_backedge_input_data :
  Node2.G.readwrite Node2.G.t ->
  (Node2.any_data Node2.phi, Node2.data) Node2.t ->
  ('a, Node2.data) Node2.t ->
  unit

val add_backedge_input_mem :
  Node2.G.readwrite Node2.G.t ->
  (Node2.any_mem Node2.phi, Node2.mem) Node2.t ->
  ('a, Node2.mem) Node2.t ->
  unit

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
val add_input : Node2.G.readwrite Node2.G.t -> ('a Node2.phi, 'b) Node2.t -> 'a -> unit
