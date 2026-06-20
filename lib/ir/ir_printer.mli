val to_dot : Node.G.readonly Node.G.t -> string

(** Export the graph to DOT format string. Data nodes are shown as circles, Control nodes as
    rectangles. *)

val to_dot_machine : Machine_node.G.readonly Machine_node.G.t -> string
val to_string_linear : Node.G.readonly Node.G.t -> string

val to_string_machine_linear :
  Machine_node.G.readonly Machine_node.G.t -> Machine_node.any list -> string

val to_string_machine_linear_regs :
  Machine_node.G.readonly Machine_node.G.t ->
  Machine_node.any list ->
  (Machine_node.any, Registers.loc) Base.Hashtbl.t ->
  string

val pp_dot : Format.formatter -> Node.G.readonly Node.G.t -> unit
val pp_dot_machine : Format.formatter -> Machine_node.G.readonly Machine_node.G.t -> unit

val pp_machine_compact :
  ?reg_assoc:(Machine_node.any, Registers.loc) Base.Hashtbl.t ->
  Machine_node.G.readonly Machine_node.G.t ->
  Format.formatter ->
  ('a, 'b) Machine_node.t ->
  unit

val pp_machine_linear :
  Format.formatter -> Machine_node.G.readonly Machine_node.G.t * Machine_node.any list -> unit

val pp_machine_linear_regs :
  Format.formatter ->
  Machine_node.G.readonly Machine_node.G.t
  * Machine_node.any list
  * (Machine_node.any, Registers.loc) Base.Hashtbl.t ->
  unit
