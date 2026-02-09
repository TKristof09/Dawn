open Core

let rec do_statement g (s : Ast.statement Ast.node) scope cur_ret_node linker =
    let loc = s.loc in
    match s.node with
    | Ast.ExprStatement e -> do_expr g e scope cur_ret_node linker |> ignore
    | Ast.Declaration_assign (name, _typ, e) ->
        let n = do_expr g e scope cur_ret_node linker |> Option.value_exn in
        Scope_node.define g scope name n
    | Ast.Declaration (name, typ) -> (
        match typ with
        | Type "i64" ->
            let default_init =
                do_expr g { loc = s.loc; node = Int 0 } scope cur_ret_node linker
                |> Option.value_exn
            in
            Scope_node.define g scope name default_init
        | Array (t, count) ->
            let element_type = Types.of_ast_type t in
            let ctrl = Scope_node.get_ctrl g scope in
            let count = do_expr g count scope cur_ret_node linker |> Option.value_exn in
            let el_size = Const_node.create_int g loc (Int.ceil_log2 8) in
            let size = Bitop_nodes.create_lsh g loc count el_size in
            let mem = Scope_node.get_mem g scope in
            let arr_type = Types.make_array element_type count.typ in
            let n = Mem_nodes.create_new g loc ~ctrl ~mem ~size arr_type in
            let mem = Proj_node.create g loc n 0 in
            let ptr = Proj_node.create g loc n 1 in
            let offset = Types.get_offset arr_type "len" |> Const_node.create_int g loc in
            let store = Mem_nodes.create_store g loc ~mem ~ptr ~offset ~value:count in
            Scope_node.define g scope name ptr;
            Scope_node.set_mem g scope store
        | _ -> failwithf "Unhandled AST type %s" (Ast.show_var_type typ) ())
    | Ast.While (cond, body) ->
        let loop_node = Loop_node.create g loc (Scope_node.get_ctrl g scope) in
        let body_scope = Scope_node.dup_loop g scope in
        Scope_node.set_ctrl g body_scope loop_node;
        Scope_node.set_ctrl g scope loop_node;
        let n_cond = do_expr g cond body_scope cur_ret_node linker |> Option.value_exn in
        let exit_scope = Scope_node.dup g body_scope in
        let n_if = If_node.create g loc ~ctrl:loop_node ~pred:n_cond in
        let n_true = Proj_node.create g loc n_if 0 in
        let n_false = Proj_node.create g loc n_if 1 in
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
            Types.make_fun_ptr
              (List.map param_types ~f:Types.of_ast_type)
              (Types.of_ast_type ret_type)
        in
        let fun_node, ret_node = Fun_node.create g loc fun_ptr_type in
        let fun_idx = Linker.define linker ~name fun_node in
        (match fun_node.kind with
        | Ctrl (Function k) -> fun_node.kind <- Ctrl (Function { k with idx = fun_idx })
        | _ -> assert false);
        let fun_ptr = Const_node.create_fun_ptr g loc fun_node fun_idx in
        Scope_node.define g scope name fun_ptr;
        Scope_node.push scope;
        let old_ctrl = Scope_node.get_ctrl g scope in
        Scope_node.set_ctrl g scope fun_node;
        List.zip_exn param_types param_names
        |> List.iteri ~f:(fun i (typ, pname) ->
            let param_type = Types.of_ast_type typ in
            let param_node = Fun_node.create_param g loc fun_node param_type i in
            Scope_node.define g scope pname param_node);
        let body_n = do_expr g body scope (Some ret_node) linker |> Option.value_exn in
        Fun_node.add_return g ret_node ~ctrl:(Scope_node.get_ctrl g scope) ~val_n:body_n;
        Scope_node.pop g scope;
        Scope_node.set_ctrl g scope old_ctrl
    | Ast.ExternalFnDeclaration (name, typ, _, external_name) ->
        let ret_type, param_types =
            match typ with
            | Ast.Fn (ret, params) -> (ret, params)
            | _ -> assert false
        in
        let ret_type = Types.of_ast_type ret_type in
        let fun_ptr_type =
            Types.make_fun_ptr (List.map param_types ~f:Types.of_ast_type) ret_type
        in
        let fun_node, ret_node = Fun_node.create g loc fun_ptr_type in
        let fun_idx = Linker.define linker ~name:external_name fun_node in
        (match fun_node.kind with
        | Ctrl (Function k) -> fun_node.kind <- Ctrl (Function { k with idx = fun_idx })
        | _ -> assert false);
        let fun_ptr = Const_node.create_fun_ptr g loc fun_node fun_idx in
        let ret_val = Extern_node.create g loc ret_type external_name in
        List.mapi param_types ~f:(fun i typ ->
            let param_type = Types.of_ast_type typ in
            Some (Fun_node.create_param g loc fun_node param_type i))
        |> Graph.add_dependencies g ret_val;
        Fun_node.add_return g ret_node ~ctrl:fun_node ~val_n:ret_val;
        Scope_node.define g scope name fun_ptr

and do_expr g (e : Ast.expr Ast.node) scope cur_ret_node linker =
    let loc = e.loc in
    let binop lhs rhs f =
        let lhs = do_expr g lhs scope cur_ret_node linker |> Option.value_exn in
        let rhs = do_expr g rhs scope cur_ret_node linker |> Option.value_exn in
        Some (f g loc lhs rhs)
    in
    match e.node with
    | Ast.Int i -> Some (Const_node.create_int g loc i)
    | Ast.Bool b -> Some (Const_node.create_int g loc (Bool.to_int b))
    | Ast.String s -> Some (Const_node.create_string g loc s)
    | Ast.Variable (name, idx_expr) -> (
        match idx_expr with
        | None ->
            let node = Scope_node.get g scope name in
            Some node
        | Some idx_expr ->
            let mem = Scope_node.get_mem g scope in
            let ptr = Scope_node.get g scope name in

            let index = do_expr g idx_expr scope cur_ret_node linker |> Option.value_exn in
            (* TODO: for now we only do 64 bit values. This cause a problem with strings for now. *)
            let el_size = Const_node.create_int g loc (Int.ceil_log2 8) in
            let base =
                match ptr.typ with
                | Ptr (Struct _ as s) -> Types.get_offset s "[]"
                | _ -> assert false
            in
            let offset =
                Arithmetic_nodes.create_add g loc
                  (Const_node.create_int g loc base)
                  (Bitop_nodes.create_lsh g loc index el_size)
            in
            let load = Mem_nodes.create_load g loc ~mem ~ptr ~offset in
            Some load)
    | Ast.VarAssign (name, expr) ->
        let n = do_expr g expr scope cur_ret_node linker |> Option.value_exn in
        Scope_node.assign g scope name n;
        Some n
    | Ast.ArrayVarAssign (name, index, value) ->
        let index = do_expr g index scope cur_ret_node linker |> Option.value_exn in
        let value = do_expr g value scope cur_ret_node linker |> Option.value_exn in
        let ptr = Scope_node.get g scope name in
        let mem = Scope_node.get_mem g scope in
        (* TODO: for now we only do 64 bit values. This cause a problem with strings for now. *)
        let el_size = Const_node.create_int g loc (Int.ceil_log2 8) in
        let base =
            match ptr.typ with
            | Ptr (Struct _ as s) -> Types.get_offset s "[]"
            | _ -> failwithf "Invalid ptr type: %s for %s" (Node.show ptr) name ()
        in
        let offset =
            Arithmetic_nodes.create_add g loc
              (Const_node.create_int g loc base)
              (Bitop_nodes.create_lsh g loc index el_size)
        in
        let store_mem = Mem_nodes.create_store g loc ~mem ~ptr ~offset ~value in
        Scope_node.set_mem g scope store_mem;
        Some value
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
        let n_if = If_node.create g loc ~ctrl ~pred:n_cond in
        let n_true = Proj_node.create g loc n_if 0 in
        let n_false = Proj_node.create g loc n_if 1 in
        let false_scope = Scope_node.dup g scope in
        Scope_node.set_ctrl g scope n_true;
        let body_true = do_expr g body scope cur_ret_node linker in
        Scope_node.set_ctrl g false_scope n_false;
        match else_body with
        | None ->
            Scope_node.merge g loc ~this:scope ~other:false_scope;
            None
        | Some else_body -> (
            let body_false = do_expr g else_body false_scope cur_ret_node linker in
            Scope_node.merge g loc ~this:scope ~other:false_scope;
            match (body_true, body_false) with
            | None, None -> None
            | Some body_true, Some body_false ->
                Some (Phi_node.create g loc (Scope_node.get_ctrl g scope) [ body_true; body_false ])
            | _, _ -> failwith "The two branches must have the same type"))
    | Ast.FnCall (expr, args) ->
        let fun_ptr = do_expr g expr scope cur_ret_node linker |> Option.value_exn in
        let args =
            List.map args ~f:(fun arg ->
                do_expr g arg scope cur_ret_node linker |> Option.value_exn)
        in
        let call_node, call_end =
            Fun_node.add_call g loc ~ctrl:(Scope_node.get_ctrl g scope) ~fun_ptr args
        in
        let ctrl = Proj_node.create g loc call_end 0 in
        Linker.link linker g call_node;
        Scope_node.set_ctrl g scope ctrl;
        let return_val = Proj_node.create g loc call_end 1 in
        Some return_val
    (* | Ast.Return ->  *)
    | _ -> failwithf "TODO: unimplemented %s" (Ast.show_expr e.node) ()

let of_ast ast linker =
    let loc : Ast.loc = { filename = ""; line = 0; col = 0 } in
    let start = Start_node.create loc in
    let stop = Stop_node.create loc in
    let g =
        Graph.create
          (module struct
            include Node

            let is_persistent = Node.is_scope
          end)
          start stop
    in
    let ctrl = Proj_node.create g loc start 0 in
    let mem = Proj_node.create g loc start 1 in
    let scope = Scope_node.create () in
    Scope_node.set_ctrl g scope ctrl;
    Scope_node.set_mem g scope mem;
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
