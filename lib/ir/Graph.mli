module type NODE = sig
  type ('a, 'tag) t
  type any = AnyNode : ('a, 'tag) t -> any

  val id : ('a, 'tag) t -> int
  val inputs_of : ('a, 'tag) t -> 'a -> any option list
  val type_eq : ('a, 'taga) t -> ('b, 'tagb) t -> (('a, 'b) Type.eq * ('taga, 'tagb) Type.eq) option
end

module type S = sig
  module N : NODE

  type readonly
  type readwrite
  type 'q t

  val create : unit -> readwrite t
  val add_node : readwrite t -> ('a, 't) N.t -> 'a -> unit
  val set_node_inputs : readwrite t -> ('a, 't) N.t -> 'a -> unit
  val remove_node : readwrite t -> ('a, 't) N.t -> unit
  val get_dependencies : 'q t -> ('a, 't) N.t -> 'a option
  val get_dependencies_exn : 'q t -> ('a, 't) N.t -> 'a
  val get_dependencies_list : 'q t -> ('a, 't) N.t -> N.any option list
  val get_dependants : 'q t -> ('a, 't) N.t -> N.any list
  val readonly : 'q t -> readonly t
  val iter : 'q t -> f:(N.any -> unit) -> unit
  val fold : 'q t -> init:'c -> f:('c -> N.any -> 'c) -> 'c
  val get_num_nodes : 'q t -> int
end

module Make : (N : NODE) -> S with module N := N

type readonly
type readwrite
type ('a, 'b) t

module type GraphNode = sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val semantic_equal : t -> t option list -> t -> t option list -> bool
  val hash : t -> int
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t

  (* TODO: This is for scope nodes so they don't get removed for not having any dependants. It feels very hacky though so i don't like it *)
  val is_persistent : t -> bool
end

val create : (module GraphNode with type t = 'a) -> 'a -> 'a -> ('a, readwrite) t
val readonly : ('a, 'b) t -> ('a, readonly) t
val get_start : ('a, 'b) t -> 'a
val get_stop : ('a, 'b) t -> 'a
val set_stop_ctrl : ('a, readwrite) t -> 'a -> unit

val add_dependencies : ('a, readwrite) t -> 'a -> 'a option list -> unit
(** Add dependencies of a node to the graph, the node doesn't need to have been added to the graph
    already.

    If the node already had dependencies, the new ones are put at the end *)

val set_dependency : ('a, readwrite) t -> 'a -> 'a option -> int -> unit
val remove_dependency : ('a, readwrite) t -> node:'a -> dep:'a -> unit
val replace_node_with : ('a, readwrite) t -> 'a -> 'a -> unit
val mem : ('a, 'b) t -> 'a -> bool
val iter : ('a, 'b) t -> f:('a -> unit) -> unit
val fold : ('a, 'b) t -> init:'c -> f:('c -> 'a -> 'c) -> 'c
val find : ('a, 'b) t -> f:('a -> bool) -> 'a option
val get_dependencies : ('a, 'b) t -> 'a -> 'a option list
val get_dependency : ('a, 'b) t -> 'a -> int -> 'a option
val get_dependants : ('a, 'b) t -> 'a -> 'a list
val remove_node : ('a, readwrite) t -> 'a -> unit
val get_num_nodes : ('a, 'b) t -> int
val finalize_node : ('a, readwrite) t -> 'a -> 'a
val cleanup : ('a, readwrite) t -> unit

val partition :
  ('a, 'b) t ->
  f:('a -> int) ->
  get_start:('a -> bool) ->
  get_stop:('a -> bool) ->
  ('a, 'readwrite) t list
