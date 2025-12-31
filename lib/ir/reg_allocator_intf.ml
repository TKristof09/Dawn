module type S = sig
  val allocate :
    Machine_node.t Graph.t ->
    Machine_node.t list list ->
    Machine_node.t list * (Machine_node.t, Registers.reg) Core.Hashtbl.t
end
