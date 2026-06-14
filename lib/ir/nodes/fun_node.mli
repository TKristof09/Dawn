val create :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  Types.t ->
  (Node2.fun_def, Node2.ctrl) Node2.t * (Node2.ret, Node2.ctrl) Node2.t

val create_param :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  (Node2.fun_def, Node2.ctrl) Node2.t ->
  Types.t ->
  int ->
  (Node2.any_data Node2.phi, Node2.data) Node2.t

val add_call :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ctrl:('a, Node2.ctrl) Node2.t ->
  mem:('b, Node2.mem) Node2.t ->
  fun_ptr:(unit, Node2.data) Node2.t ->
  ('c, Node2.data) Node2.t list ->
  (Node2.fun_call, Node2.ctrl) Node2.t * (Node2.fun_call_end, Node2.ctrl) Node2.t

val add_return :
  ?parent_fun:int ->
  Node2.G.readwrite Node2.G.t ->
  (Node2.ret, Node2.ctrl) Node2.t ->
  ctrl:('a, Node2.ctrl) Node2.t ->
  mem:('b, Node2.mem) Node2.t ->
  val_n:('c, Node2.data) Node2.t ->
  unit

val get_signature : (Node2.fun_def, Node2.ctrl) Node2.t -> Types.t

val link_call :
  Node2.G.readwrite Node2.G.t ->
  call_node:(Node2.fun_call, Node2.ctrl) Node2.t ->
  fun_node:(Node2.fun_def, Node2.ctrl) Node2.t ->
  unit

val get_call_fun_ptr :
  'a Node2.G.t -> (Node2.fun_call, Node2.ctrl) Node2.t -> (unit, Node2.data) Node2.t

val compute_fun_node_type :
  Node2.G.readonly Node2.G.t ->
  (Node2.fun_def, Node2.ctrl) Node2.t ->
  (new_type:Types.t * extra_deps:Node2.any list)

val compute_call_end_type :
  Node2.G.readonly Node2.G.t ->
  (Node2.fun_call_end, Node2.ctrl) Node2.t ->
  (new_type:Types.t * extra_deps:Node2.any list)
