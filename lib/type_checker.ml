open Core
open Ast
module H = Hashtbl.Make (String)

exception SymbolNotFound of loc * name
exception FunctionRedefinition of loc * name
exception InvalidType of loc * var_type * var_type
exception InvalidReturnType of loc * var_type * var_type
exception BranchTypeMismatch of loc * var_type * var_type
exception InvalidArgumentNum of loc * int * int

type env = {
    parent : env option;
    symbols : var_type H.t;
  }

let rec find_type loc sym env =
    match Hashtbl.find env.symbols sym with
    | Some t -> t
    | None -> (
        match env.parent with
        | Some parent -> find_type loc sym parent
        | None -> raise (SymbolNotFound (loc, sym)))

let add_symbol env name typ = Hashtbl.set env.symbols ~key:name ~data:typ

let check ast =
    let rec check_statement node env =
        match node.node with
        | Declaration (name, t) ->
            add_symbol env name t;
            env
        | Declaration_assign (name, t, expr) ->
            let assign_type = check_expr expr env in
            if equal_var_type assign_type t then (
              add_symbol env name t;
              env)
            else
              raise (InvalidType (expr.loc, t, assign_type))
        | FnDeclaration (name, t, param_names, body) -> (
            (* fn definition only in global scope for now *)
            assert (Option.is_none env.parent);
            match t with
            | Fn (ret, param_types) ->
                if Hashtbl.mem env.symbols name then
                  raise (FunctionRedefinition (node.loc, name))
                else
                  let body_env = { parent = Some env; symbols = H.create () } in
                  List.iter2_exn param_names param_types ~f:(fun id typ ->
                      add_symbol body_env id typ);
                  let body_type = check_expr body body_env in
                  if equal_var_type ret body_type then (
                    add_symbol env name t;
                    env)
                  else
                    raise (InvalidReturnType (node.loc, ret, body_type))
            | _ -> assert false)
        | ExprStatement expr ->
            check_expr expr env |> ignore;
            env
        | While (cond, body) ->
            let cond_type = check_expr cond env in
            if Ast.equal_var_type cond_type (Type "bool") then
              let body_type = check_expr body env in
              if Ast.equal_var_type body_type (Type "void") then
                env
              else
                raise (InvalidType (node.loc, Type "void", body_type))
            else
              raise (InvalidType (node.loc, Type "bool", cond_type))
    and check_expr node env =
        match node.node with
        | String _ -> Type "str"
        | Int _ -> Type "int"
        | Bool _ -> Type "bool"
        | Nullptr -> Type "nullptr"
        | Variable (name, index_expr) -> (
            let typ =
                match find_type node.loc name env with
                | Ast.Array (t, _) -> t
                | t -> t
            in
            match index_expr with
            | None -> typ
            | Some e -> (
                match check_expr e env with
                | Type "int" -> typ
                | t -> raise (InvalidType (e.loc, Type "int", t))))
        | Add (lhs, rhs)
        | Sub (lhs, rhs)
        | Mul (lhs, rhs)
        | Div (lhs, rhs)
        | Lsh (lhs, rhs)
        | Rsh (lhs, rhs)
        | BAnd (lhs, rhs)
        | BOr (lhs, rhs) -> (
            match check_expr lhs env with
            | Type "int" -> (
                match check_expr rhs env with
                | Type "int" -> Type "int"
                | t -> raise (InvalidType (rhs.loc, Type "int", t)))
            | t -> raise (InvalidType (lhs.loc, Type "int", t)))
        | Gt (lhs, rhs)
        | GEq (lhs, rhs)
        | Lt (lhs, rhs)
        | LEq (lhs, rhs)
        | Eq (lhs, rhs)
        | NEq (lhs, rhs) -> (
            match check_expr lhs env with
            | Type "int" -> (
                match check_expr rhs env with
                | Type "int" -> Type "bool"
                | t -> raise (InvalidType (rhs.loc, Type "int", t)))
            | t -> raise (InvalidType (lhs.loc, Type "int", t)))
        | UNot e -> (
            match check_expr e env with
            | Type "bool" -> Type "bool"
            | t -> raise (InvalidType (e.loc, Type "bool", t)))
        | VarAssign (name, expr) ->
            let t = find_type node.loc name env in
            let assign_type = check_expr expr env in
            if equal_var_type assign_type t then
              t
            else
              raise (InvalidType (expr.loc, t, assign_type))
        | ArrayVarAssign (name, index_expr, expr) -> (
            let t =
                match find_type node.loc name env with
                | Ast.Array (t, _) -> t
                | _ -> assert false
            in
            match check_expr index_expr env with
            | Type "int" ->
                let assign_type = check_expr expr env in
                if equal_var_type assign_type t then
                  t
                else
                  raise (InvalidType (expr.loc, t, assign_type))
            | x -> raise (InvalidType (index_expr.loc, Type "int", x)))
        | IfElse (cond, body, else_body) -> (
            match check_expr cond env with
            | Type "bool" ->
                let body_typ = check_expr body env in
                let else_typ =
                    match else_body with
                    | Some e -> check_expr e env
                    | None -> Type "void"
                in
                if equal_var_type body_typ else_typ then
                  body_typ
                else
                  raise (BranchTypeMismatch (node.loc, body_typ, else_typ))
            | t -> raise (InvalidType (cond.loc, Type "bool", t)))
        | Block (stmt_list, e) -> (
            let new_env = { parent = Some env; symbols = H.create () } in
            let new_env =
                List.fold stmt_list ~init:new_env ~f:(fun env s -> check_statement s env)
            in
            match e with
            | None -> Type "void"
            | Some e -> check_expr e new_env)
        | FnCall (name, args) -> (
            match find_type node.loc name env with
            | Fn (ret, params) ->
                let rec aux args_list params_list num_args num_params =
                    match (args_list, params_list) with
                    | [], [] -> ret
                    | arg :: at, param :: pt ->
                        let arg_type = check_expr arg env in
                        if equal_var_type arg_type param then
                          aux at pt (num_args + 1) (num_params + 1)
                        else
                          raise (InvalidType (arg.loc, param, arg_type))
                    | l, [] ->
                        raise (InvalidArgumentNum (node.loc, num_params, num_args + List.length l))
                    | [], l ->
                        raise (InvalidArgumentNum (node.loc, num_params + List.length l, num_args))
                in
                aux args params 0 0
            | t -> raise (InvalidType (node.loc, Fn (Type "fn", []), t)))
    in
    let env0 = { parent = None; symbols = H.create () } in
    add_symbol env0 "print" (Fn (Type "void", [ Type "str" ]));
    add_symbol env0 "print_int" (Fn (Type "void", [ Type "int" ]));
    try
      List.fold ast ~init:env0 ~f:(fun env s -> check_statement s env) |> ignore;
      true
    with
    | InvalidType (loc, expected, got) ->
        Printf.eprintf "%s:%d:%d: Type error: expected `%s` but got `%s`.\n" loc.filename loc.line
          loc.col (show_var_type expected) (show_var_type got);
        false
    | InvalidReturnType (loc, expected, got) ->
        Printf.eprintf "%s:%d:%d: Type error: expected function to return `%s` but got `%s`.\n"
          loc.filename loc.line loc.col (show_var_type expected) (show_var_type got);
        false
    | BranchTypeMismatch (loc, body_typ, else_typ) ->
        Printf.eprintf
          "%s:%d:%d: Type error: The two branches of an if expression must have the same type got \
           `%s` and `%s`.\n"
          loc.filename loc.line loc.col (show_var_type body_typ) (show_var_type else_typ);
        false
    | InvalidArgumentNum (loc, expected, got) ->
        Printf.eprintf "%s:%d:%d: Error: expected %d arguments but got %d.\n" loc.filename loc.line
          loc.col expected got;
        false
    | SymbolNotFound (loc, name) ->
        Printf.eprintf "%s:%d:%d: Undefined symbol `%s`.\n" loc.filename loc.line loc.col name;
        false
    | FunctionRedefinition (loc, name) ->
        Printf.eprintf "%s:%d:%d: Redefinition of function `%s`.\n" loc.filename loc.line loc.col
          name;
        false
