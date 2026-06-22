val emit_program :
  (Machine_node.G.readonly Machine_node.G.t
  * Reg_allocator.allocation_result
  * Machine_node.any list)
  list ->
  Linker.t ->
  string
