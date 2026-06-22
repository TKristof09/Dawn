type allocation_result = {
    max_stack_slot : int;
    reg_assoc : (Machine_node.any, Registers.loc) Core.Hashtbl.t;
  }

val allocate :
  Machine_node.G.readwrite Machine_node.G.t ->
  Machine_node.any list ->
  Machine_node.any list * allocation_result
