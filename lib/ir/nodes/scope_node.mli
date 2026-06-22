val create : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t

val define :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  string ->
  ('a, 'b) Node.t ->
  bool ->
  unit
(** [define g scope_node name n is_const] defines a symbol with name [name] for node [n] in the
    current scope of [scope_node]. If the name [name] was already queried (with e.g. [get]) before
    the ForwardRef node now gets replaced with the node [n]. *)

val assign :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  ?force:bool ->
  string ->
  ('a, 'b) Node.t ->
  unit
(** [assign g scope_node ?force name n] assigns the node [n] to name [name] in the current scope of
    [scope_node]. Raises if the symbol at [name] was defined as a constant. If [force] is specified
    it doesn't raise if the symbol [name] was a constant. If [name] was a ForwardRef it now gets
    replaced with [n].*)

val get : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> string -> Node.any
(** [get g scope_node name] returns the symbol for [name] in the most recent scope where one such
    name is found. If not found in any scopes a ForwardRef node is created for [name] and returned.
*)

val push : (Node.scope_kind, Node.misc) Node.t -> unit
(** [push scope_node] pushes a new scope *)

val pop : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> unit
(** [pop scope_node] pops the most recent scope, removing any symbols defined in the removed scope.
*)

val dup :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  (Node.scope_kind, Node.misc) Node.t
(** [dup g scope_node] returns another scope node with the exact same symbols as [scope_node]. *)

val dup_loop :
  Node.G.readwrite Node.G.t ->
  (Node.scope_kind, Node.misc) Node.t ->
  (Node.scope_kind, Node.misc) Node.t
(** [dup_loop g scope_node] returns another scope node with the exact same symbols as [scope_node].
    Additionally it sets the symbols in the duplicated node to point to the original scope node.
    This is used to automatically create phis lazily during loops. *)

val merge :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  this:(Node.scope_kind, Node.misc) Node.t ->
  other:(Node.scope_kind, Node.misc) Node.t ->
  unit

(** [merge ?parent_fun g loc ~this ~other] merges the symbols from [other] into [this]. Creates phi
    nodes with the given [loc] and [parent_fun] if necessary. *)

val merge_loop :
  ?parent_fun:int ->
  Node.G.readwrite Node.G.t ->
  this:(Node.scope_kind, Node.misc) Node.t ->
  body:(Node.scope_kind, Node.misc) Node.t ->
  exit:(Node.scope_kind, Node.misc) Node.t ->
  unit
(** [merge_loop ?parent_fun g ~this ~body ~exit] merges symbols from [body] and [exit] into [this].
    Connects the backedges of the phi nodes created during the loop. *)

val get_ctrl : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> Node.any_ctrl
(** [get_ctrl g scope] gets the current control node. *)

val set_ctrl :
  Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> ('a, Node.ctrl) Node.t -> unit
(** [set_ctrl g scope] sets the current control node. *)

val get_mem : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> Node.any_mem
(** [get_mem g scope] gets the current memory node. *)

val set_mem :
  Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> ('a, Node.mem) Node.t -> unit
(** [set_mem g scope] sets the current memory node. *)

val ret_identifier : string
val get_ret_ptr : Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> Node.any_data
(** [get_ret_ptr g scope] gets the current return value pointer. *)

val set_ret_ptr :
  Node.G.readwrite Node.G.t -> (Node.scope_kind, Node.misc) Node.t -> ('a, Node.data) Node.t -> unit
(** [set_ret_ptr g scope] sets the current return value pointer. *)
