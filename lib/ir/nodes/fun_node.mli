val create : (Node.t, Graph.readwrite) Graph.t -> Types.node_type -> Node.t * Node.t
val create_param : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Types.node_type -> int -> Node.t
val add_call : (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t -> Node.t list -> Node.t
val add_return : (Node.t, Graph.readwrite) Graph.t -> Node.t -> ctrl:Node.t -> val_n:Node.t -> unit
