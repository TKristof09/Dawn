val create_zint :
  Node2.G.readwrite Node2.G.t -> Ast.loc -> ?parent_fun:int -> Z.t -> (unit, Node2.data) Node2.t

val create_int :
  Node2.G.readwrite Node2.G.t -> Ast.loc -> ?parent_fun:int -> int -> (unit, Node2.data) Node2.t

val create_bool :
  Node2.G.readwrite Node2.G.t -> Ast.loc -> ?parent_fun:int -> bool -> (unit, Node2.data) Node2.t

val create_fun_ptr :
  Node2.G.readwrite Node2.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  (Node2.fun_def, Node2.ctrl) Node2.t ->
  int ->
  (unit, Node2.data) Node2.t

val create_string :
  Node2.G.readwrite Node2.G.t -> Ast.loc -> ?parent_fun:int -> string -> (unit, Node2.data) Node2.t

val create_from_type :
  Node2.G.readwrite Node2.G.t -> Ast.loc -> ?parent_fun:int -> Types.t -> (unit, Node2.data) Node2.t
