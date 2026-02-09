val create : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Types.node_type -> Node.t * Node.t

val create_param :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Types.node_type -> int -> Node.t

val add_call :
  (Node.t, Graph.readwrite) Graph.t ->
  Ast.loc ->
  ctrl:Node.t ->
  fun_ptr:Node.t ->
  Node.t list ->
  Node.t * Node.t

val add_return : (Node.t, Graph.readwrite) Graph.t -> Node.t -> ctrl:Node.t -> val_n:Node.t -> unit
val get_signature : Node.t -> Types.node_type
val link_call : (Node.t, Graph.readwrite) Graph.t -> call_node:Node.t -> fun_node:Node.t -> unit
val get_call_fun_ptr : (Node.t, 'a) Graph.t -> Node.t -> Node.t
