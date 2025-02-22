open Core

let rec do_statement g (s : Ast.statement Ast.node) scope =
    match s.node with
    | Ast.ExprStatement e -> do_expr g e scope
    | Ast.Declaration_assign (name, _typ, e) ->
        let n = do_expr g e scope |> Option.value_exn in
        Scope_node.define g scope name n;
        None
    | Ast.Declaration (name, typ) -> (
        match typ with
        | Type "int" ->
            let default_init = do_expr g { loc = s.loc; node = Int 0 } scope |> Option.value_exn in
            Scope_node.define g scope name default_init;
            None
        | _ -> assert false)
    | _ -> assert false

and do_expr g (e : Ast.expr Ast.node) scope =
    match e.node with
    | Ast.Int i -> Some (Const_node.create_int g i)
    | Ast.Variable (name, idx_expr) -> (
        match idx_expr with
        | None -> Some (Scope_node.get scope name)
        | Some _ -> assert false)
    | Ast.VarAssign (name, expr) ->
        let n = do_expr g expr scope |> Option.value_exn in
        Scope_node.assign g scope name n;
        Some n
    | Ast.Add (lhs, rhs) ->
        let lhs = do_expr g lhs scope |> Option.value_exn in
        let rhs = do_expr g rhs scope |> Option.value_exn in
        Some (Add_node.create g lhs rhs)
    | Ast.Sub (lhs, rhs) ->
        let lhs = do_expr g lhs scope |> Option.value_exn in
        let rhs = do_expr g rhs scope |> Option.value_exn in
        Some (Sub_node.create g lhs rhs)
    | Ast.Mul (lhs, rhs) ->
        let lhs = do_expr g lhs scope |> Option.value_exn in
        let rhs = do_expr g rhs scope |> Option.value_exn in
        Some (Mul_node.create g lhs rhs)
    | Ast.Div (lhs, rhs) ->
        let lhs = do_expr g lhs scope |> Option.value_exn in
        let rhs = do_expr g rhs scope |> Option.value_exn in
        Some (Div_node.create g lhs rhs)
    | Ast.Eq (lhs, rhs) ->
        let lhs = do_expr g lhs scope |> Option.value_exn in
        let rhs = do_expr g rhs scope |> Option.value_exn in
        Some (Eq_node.create g lhs rhs)
    | Ast.Block (statements, expr) ->
        Scope_node.push scope;
        List.iter statements ~f:(fun s -> do_statement g s scope |> ignore);
        let n = Option.value_map expr ~default:None ~f:(fun e -> do_expr g e scope) in
        Scope_node.pop g scope;
        n
    | Ast.IfElse (cond, body, else_body) -> (
        let n_cond = do_expr g cond scope |> Option.value_exn in
        let ctrl = Scope_node.get_ctrl scope in
        let n_if = If_node.create g ~ctrl ~pred:n_cond in
        let n_true = Proj_node.create g n_if 0 in
        let n_false = Proj_node.create g n_if 1 in
        let false_scope = Scope_node.dup g scope in
        Scope_node.set_ctrl g scope n_true;
        let body_true = do_expr g body scope in
        match else_body with
        | None ->
            Graph.remove_node g n_false;
            Graph.remove_node g false_scope;
            body_true
        | Some else_body -> (
            Scope_node.set_ctrl g false_scope n_false;
            let body_false = do_expr g else_body false_scope in
            Scope_node.merge g ~this:scope ~other:false_scope;
            match (body_true, body_false) with
            | None, None -> None
            | Some body_true, Some body_false ->
                Some (Phi_node.create g (Scope_node.get_ctrl scope) [ body_true; body_false ])
            | _, _ -> failwith "The two branches must have the same type"))
    | _ -> assert false

let of_ast ast =
    let g = Graph.create () in
    let scope = Scope_node.create () in
    Scope_node.set_ctrl g scope (Graph.get_start g);
    Core.List.iter ast ~f:(fun s -> do_statement g s scope |> ignore);
    let ctrl = Scope_node.get_ctrl scope in
    Stop_node.create g ctrl |> ignore;
    Graph.get_dependants g (Graph.get_start g)
    |> List.iter ~f:(fun n ->
           if Graph.get_dependants g n |> List.is_empty then Graph.remove_node g n);
    g
