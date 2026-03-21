val create_zint : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Z.t -> Node.t
val create_int : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> int -> Node.t
val create_bool : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> bool -> Node.t
val create_fun_ptr : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> int -> Node.t
val create_string : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> string -> Node.t
val create_from_type : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Types.t -> Node.t
