val create :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  Types.t ->
  (Node.fun_def, Node.ctrl) Node.t * (Node.ret, Node.ctrl) Node.t

val create_param :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  (Node.fun_def, Node.ctrl) Node.t ->
  Types.t ->
  int ->
  (Node.any_data Node.phi, Node.data) Node.t

val create_mem_param :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  (Node.fun_def, Node.ctrl) Node.t ->
  (Node.any_mem Node.phi, Node.mem) Node.t

val add_call :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  ctrl:('a, Node.ctrl) Node.t ->
  mem:('b, Node.mem) Node.t ->
  fun_ptr:('c, Node.data) Node.t ->
  Node.any_data list ->
  (Node.fun_call, Node.ctrl) Node.t * (Node.fun_call_end, Node.ctrl) Node.t

val add_return :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  (Node.ret, Node.ctrl) Node.t ->
  ctrl:('a, Node.ctrl) Node.t ->
  mem:('b, Node.mem) Node.t ->
  val_n:('c, Node.data) Node.t ->
  unit

val get_signature : (Node.fun_def, Node.ctrl) Node.t -> Types.t

val link_call :
  Node.G.readwrite Node.G.t ->
  call_node:(Node.fun_call, Node.ctrl) Node.t ->
  fun_node:(Node.fun_def, Node.ctrl) Node.t ->
  unit

val get_call_fun_ptr : 'a Node.G.t -> (Node.fun_call, Node.ctrl) Node.t -> Node.any_data

val get_param_nodes :
  Node.G.readonly Node.G.t ->
  (Node.fun_def, Node.ctrl) Node.t ->
  (Node.any_mem Node.phi, Node.mem) Node.t * (Node.any_data Node.phi, Node.data) Node.t list

val compute_fun_node_type :
  Node.G.readonly Node.G.t ->
  (Node.fun_def, Node.ctrl) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)

val compute_call_end_type :
  Node.G.readonly Node.G.t ->
  (Node.fun_call_end, Node.ctrl) Node.t ->
  (new_type:Types.t * extra_deps:Node.any list)
