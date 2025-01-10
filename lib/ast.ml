type var_type =
    | Type of string
    | Array of string * expr

and name = string
and program = statement list

and statement =
    | Declaration of name * var_type
    | Declaration_assign of name * var_type * expr
    | ExprStatement of expr
    | While of expr * expr

and expr =
    | String of string
    | Int of int
    | Bool of bool
    | Nullptr
    | Variable of name * expr option
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Lsh of expr * expr
    | Rsh of expr * expr
    | BAnd of expr * expr
    | BOr of expr * expr
    | Gt of expr * expr
    | GEq of expr * expr
    | Lt of expr * expr
    | LEq of expr * expr
    | Eq of expr * expr
    | NEq of expr * expr
    | LOr of expr * expr
    | LAnd of expr * expr
    | UMinus of expr
    | UNot of expr
    | VarAssign of name * expr
    | ArrayVarAssign of name * expr * expr
    | IfElse of expr * expr * expr option
    | Block of statement list * expr option
    | FnCall of name * expr list
[@@deriving show { with_path = false }]
