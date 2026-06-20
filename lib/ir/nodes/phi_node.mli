val create_data :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.ctrl) Node.t ->
  Node.any_data list ->
  (Node.any_data Node.phi, Node.data) Node.t

val create_mem :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.ctrl) Node.t ->
  Node.any_mem list ->
  (Node.any_mem Node.phi, Node.mem) Node.t

val create_data_no_backedge :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.ctrl) Node.t ->
  ('b, Node.data) Node.t ->
  (Node.any_data Node.phi, Node.data) Node.t

val create_mem_no_backedge :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ('a, Node.ctrl) Node.t ->
  ('b, Node.mem) Node.t ->
  (Node.any_mem Node.phi, Node.mem) Node.t

val add_backedge_input :
  Node.G.readwrite Node.G.t -> ('a Node.phi, 'b) Node.t -> ('c, 'b) Node.t -> unit

val get_backedge_input : Node.G.readonly Node.G.t -> ('a Node.phi, 'b) Node.t -> 'a option
val get_entry_edge_input : Node.G.readonly Node.G.t -> ('a Node.phi, 'b) Node.t -> 'a option
val add_input : Node.G.readwrite Node.G.t -> ('a Node.phi, 'b) Node.t -> 'a -> unit

val compute_type :
  Node.G.readonly Node.G.t ->
  ('a Node.phi, 'b) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)
