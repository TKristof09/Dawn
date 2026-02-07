val emit_program :
  ((Machine_node.t, Graph.readwrite) Graph.t
  * (Machine_node.t, Registers.loc) Core.Hashtbl.t
  * Machine_node.t list)
  list ->
  Linker.t ->
  string
