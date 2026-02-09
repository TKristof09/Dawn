val create : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t list -> Node.t
val get_ctrl : (Node.t, 'a) Graph.t -> Node.t -> Node.t
val create_no_backedge : (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> Node.t -> Node.t -> Node.t
val add_backedge_input : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t -> unit
val compute_type : (Node.t, 'a) Graph.t -> Node.t -> unit
