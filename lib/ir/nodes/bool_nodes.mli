val create_eq : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_neq : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_lt : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_leq : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_gt : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val create_geq : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
