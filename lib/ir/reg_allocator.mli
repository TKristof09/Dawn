val allocate :
  Machine_node.G.readwrite Machine_node.G.t ->
  Machine_node.any list ->
  Machine_node.any list * (Machine_node.any, Registers.loc) Core.Hashtbl.t
