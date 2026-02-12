val to_dot : (Node.t, 'a) Graph.t -> string
(** Export the graph to DOT format string. Data nodes are shown as circles, Control nodes as
    rectangles. *)

val to_dot_machine : (Machine_node.t, 'a) Graph.t -> string
val to_string_linear : (Node.t, 'a) Graph.t -> string
val to_string_machine_linear : (Machine_node.t, 'a) Graph.t -> Machine_node.t list -> string

val to_string_machine_linear_regs :
  (Machine_node.t, 'a) Graph.t ->
  Machine_node.t list ->
  (Machine_node.t, Registers.loc) Base.Hashtbl.t ->
  string

val pp_dot : Format.formatter -> (Node.t, 'a) Graph.t -> unit
val pp_dot_machine : Format.formatter -> (Machine_node.t, 'a) Graph.t -> unit

val pp_machine_compact :
  ?reg_assoc:(Machine_node.t, Registers.loc) Base.Hashtbl.t ->
  (Machine_node.t, 'a) Graph.t ->
  Format.formatter ->
  Machine_node.t ->
  unit

val pp_machine_linear :
  Format.formatter -> (Machine_node.t, 'a) Graph.t * Machine_node.t list -> unit

val pp_machine_linear_regs :
  Format.formatter ->
  (Machine_node.t, 'a) Graph.t
  * Machine_node.t list
  * (Machine_node.t, Registers.loc) Base.Hashtbl.t ->
  unit
