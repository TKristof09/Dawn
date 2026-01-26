val create : Node.t Graph.t -> Node.t -> Node.t list -> Node.t
val get_ctrl : Node.t Graph.t -> Node.t -> Node.t
val create_no_backedge : Node.t Graph.t -> Node.t -> Node.t -> Node.t
val add_backedge_input : Node.t Graph.t -> Node.t -> Node.t -> unit
val compute_type : Node.t Graph.t -> Node.t -> unit
