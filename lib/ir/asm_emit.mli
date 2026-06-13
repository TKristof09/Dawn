val emit_program :
  (Machine_node.G.readonly Machine_node.G.t
  * (Machine_node.any, Registers.loc) Core.Hashtbl.t
  * Machine_node.any list)
  list ->
  Linker.t ->
  string
