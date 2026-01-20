type loc =
    | Reg of Registers.reg
    | Stack of int
[@@deriving show { with_path = false }]

module type S = sig
  val allocate :
    Machine_node.t Graph.t ->
    Machine_node.t list list ->
    Machine_node.t list * (Machine_node.t, loc) Core.Hashtbl.t
end
