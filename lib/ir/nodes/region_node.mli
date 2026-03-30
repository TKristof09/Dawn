val create :
  (Node.t, Graph.readwrite) Graph.t -> Ast.loc -> ?parent_fun:int -> Node.t * Node.t -> Node.t

val compute_type : (Node.t, 'a) Graph.t -> Node.t -> (new_type:Types.t * extra_deps:Node.t list)
