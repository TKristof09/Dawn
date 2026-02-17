val create_lsh : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_rsh : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_band : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_bor : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
