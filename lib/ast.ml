type loc = {
    filename : string;
    line : int;
    col : int;
  }

and 'a node = {
    node : 'a;
    loc : loc;
  }

and var_type =
    | Type of string [@printer fun fmt -> fprintf fmt "%s"]
    | Array of var_type * expr node
        [@printer fun fmt (t, n) -> fprintf fmt "%s[%s]" (show_var_type t) (show_expr n.node)]
    | Fn of var_type * var_type list
        [@printer
            fun fmt (ret, params) ->
              fprintf fmt "(%s) -> %s"
                (String.concat ", " (List.map show_var_type params))
                (show_var_type ret)]

and name = string
and program = statement node list

and statement =
    | Declaration of name * var_type
    | Declaration_assign of name * var_type * expr node
    | FnDeclaration of name * var_type * name list * expr node
    | ExprStatement of expr node
    | While of expr node * expr node

and expr =
    | String of string
    | Int of int
    | Bool of bool
    | Nullptr
    | Variable of name * expr node option
    | Add of expr node * expr node
    | Sub of expr node * expr node
    | Mul of expr node * expr node
    | Div of expr node * expr node
    | Lsh of expr node * expr node
    | Rsh of expr node * expr node
    | BAnd of expr node * expr node
    | BOr of expr node * expr node
    | Gt of expr node * expr node
    | GEq of expr node * expr node
    | Lt of expr node * expr node
    | LEq of expr node * expr node
    | Eq of expr node * expr node
    | NEq of expr node * expr node
    | UNot of expr node
    | VarAssign of name * expr node
    | ArrayVarAssign of name * expr node * expr node
    | IfElse of expr node * expr node * expr node option
    | Block of statement node list * expr node option
    | FnCall of expr node * expr node list
[@@deriving show { with_path = false }, eq]
