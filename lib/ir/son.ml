open Core

let rec do_statement g (s : Ast.statement Ast.node) scope cur_ret_node linker =
    match s.node with
    | Ast.ExprStatement e -> do_expr g e scope cur_ret_node linker |> ignore
    | Ast.Declaration_assign (name, _typ, e) ->
        let n = do_expr g e scope cur_ret_node linker |> Option.value_exn in
        Scope_node.define g scope name n
    | Ast.Declaration (name, typ) -> (
        match typ with
        | Type "int" ->
            let default_init =
                do_expr g { loc = s.loc; node = Int 0 } scope cur_ret_node linker
                |> Option.value_exn
            in
            Scope_node.define g scope name default_init
        | _ -> assert false)
    | Ast.While (cond, body) ->
        let loop_node = Loop_node.create g (Scope_node.get_ctrl g scope) in
        let body_scope = Scope_node.dup_loop g scope in
        Scope_node.set_ctrl g body_scope loop_node;
        Scope_node.set_ctrl g scope loop_node;
        let n_cond = do_expr g cond body_scope cur_ret_node linker |> Option.value_exn in
        let exit_scope = Scope_node.dup g body_scope in
        let n_if = If_node.create g ~ctrl:loop_node ~pred:n_cond in
        let n_true = Proj_node.create g n_if 0 in
        let n_false = Proj_node.create g n_if 1 in
        Scope_node.set_ctrl g body_scope n_true;
        Scope_node.set_ctrl g exit_scope n_false;
        let _ = do_expr g body body_scope cur_ret_node linker in
        Loop_node.set_back_edge g loop_node (Scope_node.get_ctrl g body_scope);
        Scope_node.merge_loop g ~this:scope ~body:body_scope ~exit:exit_scope
    | Ast.FnDeclaration (name, typ, param_names, body) ->
        let ret_type, param_types =
            match typ with
            | Ast.Fn (ret, params) -> (ret, params)
            | _ -> assert false
        in
        let fun_ptr_type =
            Types.(
              FunPtr
                (Value
                   {
                     params = List.map param_types ~f:Types.of_ast_type;
                     ret = Types.of_ast_type ret_type;
                     fun_indices = `Include Int.Set.empty;
                   }))
        in
        let fun_node, ret_node = Fun_node.create g fun_ptr_type in
        let fun_idx = Linker.define linker fun_node in
        (match fun_node.kind with
        | Ctrl (Function k) -> fun_node.kind <- Ctrl (Function { k with idx = fun_idx })
        | _ -> assert false);
        let fun_ptr = Const_node.create_fun_ptr g fun_node fun_idx in
        Scope_node.define g scope name fun_ptr;
        Scope_node.push scope;
        let old_ctrl = Scope_node.get_ctrl g scope in
        Scope_node.set_ctrl g scope fun_node;
        List.zip_exn param_types param_names
        |> List.iteri ~f:(fun i (typ, pname) ->
            let param_type = Types.of_ast_type typ in
            let param_node = Fun_node.create_param g fun_node param_type i in
            Scope_node.define g scope pname param_node);
        let body_n = do_expr g body scope (Some ret_node) linker |> Option.value_exn in
        Fun_node.add_return g ret_node ~ctrl:(Scope_node.get_ctrl g scope) ~val_n:body_n;
        Scope_node.pop g scope;
        Scope_node.set_ctrl g scope old_ctrl

and do_expr g (e : Ast.expr Ast.node) scope cur_ret_node linker =
    let binop lhs rhs f =
        let lhs = do_expr g lhs scope cur_ret_node linker |> Option.value_exn in
        let rhs = do_expr g rhs scope cur_ret_node linker |> Option.value_exn in
        Some (f g lhs rhs)
    in
    match e.node with
    | Ast.Int i -> Some (Const_node.create_int g i)
    | Ast.Bool b -> Some (Const_node.create_int g (Bool.to_int b))
    | Ast.Variable (name, idx_expr) -> (
        match idx_expr with
        | None ->
            let node = Scope_node.get g scope name in
            Some node
        | Some _ -> assert false)
    | Ast.VarAssign (name, expr) ->
        let n = do_expr g expr scope cur_ret_node linker |> Option.value_exn in
        Scope_node.assign g scope name n;
        Some n
    | Ast.Add (lhs, rhs) -> binop lhs rhs Arithmetic_nodes.create_add
    | Ast.Sub (lhs, rhs) -> binop lhs rhs Arithmetic_nodes.create_sub
    | Ast.Mul (lhs, rhs) -> binop lhs rhs Arithmetic_nodes.create_mul
    | Ast.Div (lhs, rhs) -> binop lhs rhs Arithmetic_nodes.create_div
    | Ast.Lsh (lhs, rhs) -> binop lhs rhs Bitop_nodes.create_lsh
    | Ast.Rsh (lhs, rhs) -> binop lhs rhs Bitop_nodes.create_rsh
    | Ast.BAnd (lhs, rhs) -> binop lhs rhs Bitop_nodes.create_band
    | Ast.BOr (lhs, rhs) -> binop lhs rhs Bitop_nodes.create_bor
    | Ast.Eq (lhs, rhs) -> binop lhs rhs Bool_nodes.create_eq
    | Ast.NEq (lhs, rhs) -> binop lhs rhs Bool_nodes.create_neq
    | Ast.Lt (lhs, rhs) -> binop lhs rhs Bool_nodes.create_lt
    | Ast.LEq (lhs, rhs) -> binop lhs rhs Bool_nodes.create_leq
    | Ast.Gt (lhs, rhs) -> binop lhs rhs Bool_nodes.create_gt
    | Ast.GEq (lhs, rhs) -> binop lhs rhs Bool_nodes.create_geq
    | Ast.Block (statements, expr) ->
        Scope_node.push scope;
        List.iter statements ~f:(fun s -> do_statement g s scope cur_ret_node linker |> ignore);
        let n =
            Option.value_map expr ~default:None ~f:(fun e -> do_expr g e scope cur_ret_node linker)
        in
        Scope_node.pop g scope;
        n
    | Ast.IfElse (cond, body, else_body) -> (
        let n_cond = do_expr g cond scope cur_ret_node linker |> Option.value_exn in
        let ctrl = Scope_node.get_ctrl g scope in
        let n_if = If_node.create g ~ctrl ~pred:n_cond in
        let n_true = Proj_node.create g n_if 0 in
        let n_false = Proj_node.create g n_if 1 in
        let false_scope = Scope_node.dup g scope in
        Scope_node.set_ctrl g scope n_true;
        let body_true = do_expr g body scope cur_ret_node linker in
        Scope_node.set_ctrl g false_scope n_false;
        match else_body with
        | None ->
            Scope_node.merge g ~this:scope ~other:false_scope;
            None
        | Some else_body -> (
            let body_false = do_expr g else_body false_scope cur_ret_node linker in
            Scope_node.merge g ~this:scope ~other:false_scope;
            match (body_true, body_false) with
            | None, None -> None
            | Some body_true, Some body_false ->
                Some (Phi_node.create g (Scope_node.get_ctrl g scope) [ body_true; body_false ])
            | _, _ -> failwith "The two branches must have the same type"))
    | Ast.FnCall (expr, args) ->
        let fun_ptr = do_expr g expr scope cur_ret_node linker |> Option.value_exn in
        let args =
            List.map args ~f:(fun arg ->
                do_expr g arg scope cur_ret_node linker |> Option.value_exn)
        in
        let call_node, call_end =
            Fun_node.add_call g ~ctrl:(Scope_node.get_ctrl g scope) ~fun_ptr args
        in
        let ctrl = Proj_node.create g call_end 0 in
        Linker.link linker g call_node;
        Scope_node.set_ctrl g scope ctrl;
        let return_val = Proj_node.create g call_end 1 in
        Some return_val
    (* | Ast.Return ->  *)
    | _ -> assert false

let of_ast ast =
    let start = Start_node.create () in
    let stop = Stop_node.create () in
    let g =
        Graph.create
          (module struct
            include Node

            let is_persistent = Node.is_scope
          end)
          start stop
    in
    let linker = Linker.create () in
    let scope = Scope_node.create () in
    Scope_node.set_ctrl g scope (Graph.get_start g);
    Core.List.iter ast ~f:(fun s -> do_statement g s scope None linker);
    let ctrl = Scope_node.get_ctrl g scope in
    Graph.set_stop_ctrl g ctrl;
    Scope_node.set_ctrl g scope (Graph.get_stop g);
    (* makes life easier and we dont need the scope anymore i think*)
    (* Graph.remove_node g scope; *)
    (* Graph.add_dependencies g stop (Graph.get_dependencies g scope |> List.tl_exn); *)
    (* Graph.cleanup g; *)
    Graph.get_dependants g (Graph.get_start g)
    |> List.iter ~f:(fun n ->
        if Graph.get_dependants g n |> List.is_empty then Graph.remove_node g n);
    g
