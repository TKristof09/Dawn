val to_dot : Node.t Graph.t -> string
(** Export the graph to DOT format string. Data nodes are shown as circles, Control nodes as
    rectangles. *)

val to_dot_machine : Machine_node.t Graph.t -> string
val to_string_linear : Node.t Graph.t -> string
val to_string_machine_linear : Machine_node.t Graph.t -> Machine_node.t list -> string

val to_string_machine_linear_regs :
  Machine_node.t Graph.t ->
  Machine_node.t list ->
  (Machine_node.t, Registers.loc) Base.Hashtbl.t ->
  string
