module type S = sig
  val allocate : Node.t Graph.t -> Machine_node.t list -> Machine_node.t list
end
