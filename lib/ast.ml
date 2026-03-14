open Sexplib0.Sexp_conv

type loc = {
    filename : string;
    line : int;
    col : int;
  }

and qualifier =
    | Const
    | Mutable

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
              fprintf fmt "fun((%s) -> %s)"
                (String.concat ", " (List.map show_var_type params))
                (show_var_type ret)]
    | Struct of (string * var_type) list
        [@printer
            fun fmt fields ->
              fprintf fmt "struct {%a}"
                (Format.pp_print_list
                   ~pp_sep:(fun fmt () -> fprintf fmt "; ")
                   (fun fmt (name, typ) -> fprintf fmt "%s : %s" name (show_var_type typ)))
                fields]

and name = string
and program = statement node list

and statement =
    | Declaration of name * var_type
    | Declaration_assign of name * var_type * expr node * qualifier
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
    | FnDeclaration of var_type * name list * expr node
    | ExternalFnDeclaration of var_type * name list * string
    | TypeDeclaration of var_type
    | TypeInstantiation of name * (name option * expr node) list
        [@equal
            fun (type_name, l) (type_name', l') ->
              name_equal type_name type_name'
              && List.equal
                   (fun (field_name, value) (field_name', value') ->
                     Option.equal name_equal field_name field_name' && node_equal value value')
                   l l']
    | FieldAccess of expr node * name
[@@deriving show { with_path = false }, eq, sexp_of]

let compare_loc l l' =
    let c = String.compare l.filename l'.filename in
    if c <> 0 then
      c
    else
      let c = Int.compare l.line l'.line in
      if c <> 0 then
        c
      else
        Int.compare l.col l'.col
