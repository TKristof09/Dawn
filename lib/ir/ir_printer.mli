val to_dot : Node.t Graph.t -> string
(** Export the graph to DOT format string. Data nodes are shown as circles, Control nodes as
    rectangles. *)

val to_dot_machine : Machine_node.t Graph.t -> string
