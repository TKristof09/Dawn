(** Bidirectional directed graph.

    The graph can return node inputs in a type safe manner.

    Both getting the incoming edges to a node and getting the outgoing edges are O(1) amortised
    operations. *)

module type NODE = sig
  type ('a, 'tag) t
  type any = AnyNode : ('a, 'tag) t -> any

  val id : ('a, 'tag) t -> int
  (** [id node] returns a numeric id for the node. The id should be unique *)

  val list_of_inputs : ('a, 'tag) t -> 'a -> any option list
  (** [list_of_inputs node inputs] Convert the node's structured input data structure into a flat
      list

      Inverse of [inputs_of_list]. *)

  val inputs_of_list : ('a, 'tag) t -> any option list -> 'a
  (** [inputs_of_list node l] Convert the node's inputs given in flat list representation to the
      structured type

      Inverse of [list_of_inputs]. *)

  val type_eq : ('a, 'taga) t -> ('b, 'tagb) t -> (('a, 'b) Type.eq * ('taga, 'tagb) Type.eq) option
  (** Helper for type equality witness. *)
end

module type S = sig
  module N : NODE

  type readonly
  type readwrite
  type 'q t

  val create : start:N.any -> stop:N.any -> readwrite t
  (** [create start stop ] initialises the graph with the start and stop nodes *)

  val readonly : 'q t -> readonly t
  (** [readonly g] returns a readonly version of g. This only changes the type. *)

  val add_node : readwrite t -> ('a, 't) N.t -> 'a -> unit
  (** [add_node g n inputs] adds the node [n] to [g] along with its [inputs].

      [n] must not be already in [g]. The nodes in [input] do not have to already be in [g] *)

  val set_node_inputs : readwrite t -> ('a, 't) N.t -> 'a -> unit
  (** [set_node_inputs g n inputs] overwrites [n]'s inputs in the graph [g].

      [n] must already be part of [g] *)

  val set_ctrl : readwrite t -> ('a, 'ta) N.t -> ('b, 'tb) N.t -> unit
  (** [set_ctrl g n ctrl] sets the control input of [n] to [ctrl].

      [n] must already be part of [g] *)

  val unlink_ctrl : readwrite t -> ('a, 'ta) N.t -> unit
  (** [unlink_ctrl g n] sets n's control input to None.

      [n] must already be part of [g] *)

  val remove_node : readwrite t -> ('a, 't) N.t -> unit
  (** [remove_node g n] removes [n] from [g]

      [n] must already be part of [g].

      If [n] was the last user of any of its inputs, the now unused nodes are also removed. *)

  val replace_node_with : readwrite t -> from:('a, 't) N.t -> to_:('a, 't) N.t -> unit
  (** [replace_node_with g from to_] replaces [from] with [to_] in the graph. This entails removing
      all inputs of [from] (which potentially removes some nodes if [from] was their only user) and
      setting all users of [from] to be users of [to_] instead.

      This is equivalent to calling [replace_input g ~node:user ~from ~to] on every [user] of [from]
      and then calling [remove_node g from]. *)

  val replace_node_with_unsafe : readwrite t -> from:N.any -> to_:N.any -> unit
  (** [replace_node_with g from to_] replaces [from] with [to_] in the graph. This entails removing
      all inputs of [from] (which potentially removes some nodes if [from] was their only user) and
      setting all users of [from] to be users of [to_] instead.

      This is equivalent to calling [replace_input_unsafe g ~node:user ~from ~to] on every [user] of
      [from] and then calling [remove_node g from].

      This is the unsafe version. It is up to the user to make sure that the replacement is
      compatible with what the node expects as inputs. *)

  val get_start : 'q t -> N.any
  (** [get_start g] returns the start node that was given during graph creation *)

  val get_stop : 'q t -> N.any
  (** [get_start g] returns the start node that was given during graph creation *)

  val get_ctrl : 'q t -> ('a, 't) N.t -> N.any option
  (** [get_ctrl g n] returns the control input of [n] if set, otherwise None. If [n] is not part of
      the graph it returns None as well. *)

  val get_ctrl_exn : 'q t -> ('a, 't) N.t -> N.any
  (** [get_ctrl_exn g n] returns the control input of [n] if set, raises if the control input is not
      set or if [n] is not part of [g]*)

  val get_dependencies : 'q t -> ('a, 't) N.t -> 'a option
  (** [get_dependencies g n] returns the inputs of [n]. Returns None if [n] is not part of [g] *)

  val get_dependencies_exn : 'q t -> ('a, 't) N.t -> 'a
  (** [get_dependencies_exn g n] returns the inputs of [n]. Raises if [n] is not part of [g] *)

  val get_dependencies_list : 'q t -> ('a, 't) N.t -> N.any option list
  (** [get_dependencies_list g n] returns the flat list representation of the inputs of [n]. Returns
      the empty list if [n] is not part of [g].

      The order of nodes in the list corresponds to the order in N.list_of_inputs for the node n. *)

  val get_dependants : 'q t -> ('a, 't) N.t -> N.any list
  (** [get_dependants g n] returns the list of users of [n]. Returns the empty list if [n] is not
      part of [g]

      The order of nodes in the returned list is not guaranteed. *)

  val replace_input :
    readwrite t -> node:('a, 'ta) N.t -> from:('b, 'tb) N.t -> to_:('b, 'tb) N.t -> unit
  (** [replace_input g node from to_] replaces all occurrences of [from] in [node]'s inputs with
      [to_] *)

  val replace_input_unsafe : readwrite t -> node:('a, 'ta) N.t -> from:N.any -> to_:N.any -> unit
  (** [replace_input_unsafe g node from to_] replaces all occurrences of [from] in [node]'s inputs
      with [to_]

      This is the unsafe version. It is up to the user to make sure that the replacement is
      compatible with what the node expects as inputs. *)

  val partition :
    'q t ->
    f:(N.any -> int) ->
    get_start:(N.any -> bool) ->
    get_stop:(N.any -> bool) ->
    readwrite t list
  (** [partition g f get_start get_stop] partitions the graph [g]. Nodes are split based on the
      result of the function [f], [n] and [n'] will be part of the same partition if [f n = f n'].
      Each new graph's start and stop nodes are chosen based on [get_start] and [get_stop]
      functions.

      [get_start] and [get_stop] should only return [true] for a single node in each partition
      (otherwise the start and stop nodes are arbitrarily chosen among nodes with true result) *)

  val mem : 'q t -> N.any -> bool
  (** [mem g n] returns true if [n] is part of [g], false otherwise *)

  val iter : 'q t -> f:(N.any -> unit) -> unit
  (** [iter g f] applies [f] to every node in the graph. [f] should not modify the graph during
      iteration.

      Order of iteration has no guarantees. *)

  val fold : 'q t -> init:'c -> f:('c -> N.any -> 'c) -> 'c
  (** [f] should not modify the graph during iteration.

      Order of iteration has no guarantees. *)

  val find : 'q t -> f:(N.any -> bool) -> N.any option
  (** [find g f] returns a node [n] that satisfies [f]. Returns None if none found *)

  val find_map : 'q t -> f:(N.any -> 'a option) -> 'a option
  (** [find_map g f] returns [f n] if [f] returns Some on [n]. Returns None if none found *)

  val get_num_nodes : 'q t -> int
  (** [get_num_nodes g] returns the number of nodes in the graph [g] *)
end

module Make : (N : NODE) -> S with module N := N
