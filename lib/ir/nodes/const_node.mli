val create_zint :
  Node.G.readwrite Node.G.t -> Ast.loc -> ?parent_fun:int -> Z.t -> (unit, Node.data) Node.t

val create_int :
  Node.G.readwrite Node.G.t -> Ast.loc -> ?parent_fun:int -> int -> (unit, Node.data) Node.t

val create_bool :
  Node.G.readwrite Node.G.t -> Ast.loc -> ?parent_fun:int -> bool -> (unit, Node.data) Node.t

val create_fun_ptr :
  Node.G.readwrite Node.G.t ->
  Ast.loc ->
  ?parent_fun:int ->
  (Node.fun_def, Node.ctrl) Node.t ->
  int ->
  (unit, Node.data) Node.t

val create_string :
  Node.G.readwrite Node.G.t -> Ast.loc -> ?parent_fun:int -> string -> (unit, Node.data) Node.t

val create_from_type :
  Node.G.readwrite Node.G.t -> Ast.loc -> ?parent_fun:int -> Types.t -> (unit, Node.data) Node.t
