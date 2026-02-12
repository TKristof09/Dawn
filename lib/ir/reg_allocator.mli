val allocate :
  (Machine_node.t, Graph.readwrite) Graph.t ->
  Machine_node.t list ->
  Machine_node.t list * (Machine_node.t, Registers.loc) Core.Hashtbl.t
