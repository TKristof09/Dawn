val create_zint : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> Z.t -> Node.t
val create_int : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> int -> Node.t
val create_bool : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> bool -> Node.t

val create_fun_ptr :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> Node.t -> int -> Node.t

val create_string :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> string -> Node.t

val create_from_type :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> Types.t -> Node.t
