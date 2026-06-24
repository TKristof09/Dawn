open Core

let rec get_type g scope t =
    match t with
    | Ast.Type "Self" -> Types.Self
    | Ast.Type s -> (
        assert (not (String.is_empty s));
        let (AnyNode n) = Scope_node.get g scope s in
        match n.typ with
        | Type (Value t) -> t
        | _ -> failwithf "todo: i think this should be an error: %s" (Node.show n) ())
    | Fn _ -> Types.of_ast_type t
    | Array (t, _) ->
        (* TODO: perhaps we want to keep the count known in the future to check
           against it in type_check. But for now we ignore type annotations
           anyway *)
        Types.make_array (get_type g scope t) Types.i64
    | _ -> failwith "todo"

let rec do_statement g (s : Ast.statement Ast.node) scope parent_fun cur_ret_node linker =
    let loc = s.loc in
    match s.node with
    | Ast.ExprStatement e -> do_expr g e scope parent_fun cur_ret_node linker |> ignore
    | Ast.Declaration_assign (name, typ, e, qualifier) ->
        let (AnyData n) = do_expr g e scope parent_fun cur_ret_node linker |> Option.value_exn in

        (* type annotation sets the min type. TODO: ast type should be an optional later with type inference *)
        (if not (Ast.equal_var_type typ (Type "")) then
           let typ = get_type g scope typ in
           n.Node.min_typ <- Some typ);

        (match n.Node.kind with
        | Data Constant -> (
            match n.typ with
            | Types.FunPtr (Value _) ->
                let fun_idx = Types.get_fun_idx n.typ |> Option.value_exn in
                (* HACK: only rename function if it is still the default name.
                   This is to prevent renaming when assigning an existing
                   function to a variable *)
                if
                  String.equal
                    (Printf.sprintf "Anon_fn_%d" (fun_idx - 1))
                    (Linker.get_name linker fun_idx)
                then
                  Linker.set_name linker fun_idx name
            | Type (Value (Struct (Value { name = _; fields }))) ->
                n.typ <- Type (Value (Types.make_struct name fields))
            | Type (Value (Trait (Value { name = _; fields }))) ->
                n.typ <- Type (Value (Types.make_trait name fields))
            | Integer (Value i) when Option.is_some n.min_typ -> (
                (* for constant integers we set the width to the type annotation's width if present *)
                match n.min_typ |> Option.value_exn with
                | Integer (Value min_typ) ->
                    n.typ <- Types.make_int_const ?fixed_width:min_typ.fixed_width i.min
                | _ -> ())
            | _ -> ())
        | _ -> ());
        Scope_node.define g scope name n (Poly.equal qualifier Ast.Const)
    | Ast.Declaration (name, typ) -> (
        match typ with
        | Type _ -> (
            let t = get_type g scope typ in
            match t with
            | Integer _ ->
                let (AnyData default_init) =
                    do_expr g
                      { loc = s.loc; node = Int Z.zero }
                      scope parent_fun cur_ret_node linker
                    |> Option.value_exn
                in
                default_init.min_typ <- Some t;
                Scope_node.define g scope name default_init false
            | _ -> failwith "todo")
        | Array (t, count) ->
            (* TODO: use this function, but for now it doesn't fill in the count even if it's constant *)
            (* let arr_type = get_type g scope typ in *)
            let element_type = get_type g scope t in
            let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
            let (AnyData count) =
                do_expr g count scope parent_fun cur_ret_node linker |> Option.value_exn
            in
            count.min_typ <- Some Types.i64;
            let el_size = Const_node.create_int ?parent_fun g loc (Types.get_size element_type) in
            let size = Arithmetic_nodes.create_mul ?parent_fun g loc count el_size in
            let size =
                Arithmetic_nodes.create_add ?parent_fun g loc size
                  (Const_node.create_int ?parent_fun g loc (Types.get_size Types.i64))
            in
            size.min_typ <- Some Types.i64;
            let (AnyMem mem) = Scope_node.get_mem g scope in
            let count_type =
                match count.typ with
                | Integer (Value { min; max; num_widens; fixed_width }) ->
                    Types.make_int ~num_widens ~fixed_width:64 min max
                | _ -> count.typ
            in
            count.min_typ <- Some count_type;
            let arr_type = Types.make_array element_type count_type in
            let n = Mem_nodes.create_new ?parent_fun g loc ~ctrl ~mem ~size arr_type in
            let mem = Proj_node.create_mem ?parent_fun g loc n 0 in
            let ptr = Proj_node.create_data ?parent_fun g loc n 1 in
            ptr.min_typ <- Some arr_type;
            let len_ptr = Mem_nodes.create_addr_of_field ?parent_fun g loc ptr "len" in
            let store =
                Mem_nodes.create_store ?parent_fun g loc ~mem ~ptr:len_ptr "len" ~value:count
            in
            Scope_node.define g scope name ptr false;
            Scope_node.set_mem g scope store
        | _ -> failwithf "Unhandled AST type %s" (Ast.show_var_type typ) ())
    | Ast.While (cond, body) ->
        let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
        let loop_node = Loop_node.create ?parent_fun g loc ctrl in
        let body_scope = Scope_node.dup_loop g scope in
        Scope_node.set_ctrl g body_scope loop_node;
        Scope_node.set_ctrl g scope loop_node;
        let (AnyData n_cond) =
            do_expr g cond body_scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let exit_scope = Scope_node.dup g body_scope in
        let n_if = If_node.create ?parent_fun g loc ~ctrl:loop_node ~pred:n_cond in
        let n_true = Proj_node.create_ctrl ?parent_fun g loc n_if 0 in
        let n_false = Proj_node.create_ctrl ?parent_fun g loc n_if 1 in
        Scope_node.set_ctrl g body_scope n_true;
        Scope_node.set_ctrl g exit_scope n_false;
        let _ = do_expr g body body_scope parent_fun cur_ret_node linker in
        let (AnyCtrl body_ctrl) = Scope_node.get_ctrl g body_scope in
        Loop_node.set_back_edge g loop_node body_ctrl;
        Scope_node.merge_loop ?parent_fun g ~this:scope ~body:body_scope ~exit:exit_scope
    | TraitImplementation (t_base, t_trait, fields) ->
        let (AnyNode trait) = Scope_node.get g scope t_trait in
        let (AnyNode base) = Scope_node.get g scope t_base in
        let trait_fields =
            match trait.typ with
            | Type (Value (Trait (Value { name; fields }))) ->
                assert (String.equal name t_trait);
                List.map fields ~f:(fun (name, e) -> (Printf.sprintf "$%s$%s" t_trait name, e))
            | _ -> assert false
        in
        let fields =
            List.map fields ~f:(fun (name, e) -> (Printf.sprintf "$%s$%s" t_trait name, e))
        in
        let field_names = List.map trait_fields ~f:fst |> String.Set.of_list in
        let base_fields =
            match base.typ with
            | Type (Value (Struct (Value { name = _; fields }))) -> fields
            | _ -> assert false
        in
        let remaining_field_names, trait_fields =
            List.fold fields ~init:(field_names, [])
              ~f:(fun (remaining_field_names, trait_fields) (field_name, e) ->
                if not (Set.mem remaining_field_names field_name) then
                  if Set.mem field_names field_name then
                    failwithf "%s:%d: Field %s is implemented multiple times" loc.filename loc.line
                      field_name ()
                  else
                    failwithf "%s:%d: Field %s is not part of trait %s" loc.filename loc.line
                      field_name t_trait ()
                else
                  let (AnyData field) =
                      do_expr g e scope parent_fun cur_ret_node linker |> Option.value_exn
                  in
                  let fun_idx = Types.get_fun_idx field.typ |> Option.value_exn in
                  (* HACK: only rename function if it is still the default name.
                   This is to prevent renaming when assigning an existing
                   function to a variable *)
                  if
                    String.equal
                      (Printf.sprintf "Anon_fn_%d" (fun_idx - 1))
                      (Linker.get_name linker fun_idx)
                  then
                    Linker.set_name linker fun_idx (t_base ^ field_name)
                  (* field name is already mangled like "$trait_name$field_name" *);
                  Scope_node.define g scope (t_base ^ field_name) field true;
                  ( Set.remove remaining_field_names field_name,
                    (field_name, field.typ) :: trait_fields ))
        in
        if not (Set.is_empty remaining_field_names) then
          failwithf "%s:%d: Following fields were not implemented: %s" loc.filename loc.line
            (Set.to_list remaining_field_names |> String.concat ~sep:", ")
            ();
        let new_fields = base_fields @ (("$" ^ t_trait, Void) :: trait_fields) in
        let new_typ = Types.make_struct t_base new_fields in
        let new_typ_node = Const_node.create_from_type g loc ?parent_fun (Type (Value new_typ)) in
        Scope_node.assign g scope ~force:true t_base new_typ_node

and do_expr g (e : Ast.expr Ast.node) scope parent_fun cur_ret_node linker : Node.any_data option =
    let loc = e.loc in
    match e.node with
    | Ast.Int i -> Some (AnyData (Const_node.create_zint ?parent_fun g loc i))
    | Ast.Bool b -> Some (AnyData (Const_node.create_bool ?parent_fun g loc b))
    | Ast.String s -> Some (AnyData (Const_node.create_string ?parent_fun g loc s))
    | Ast.Variable (name, idx_expr) -> (
        match idx_expr with
        | None ->
            let (AnyNode node) = Scope_node.get g scope name in
            let node = Node.as_data_exn node in
            Some (AnyData node)
        | Some idx_expr ->
            let (AnyMem mem) = Scope_node.get_mem g scope in
            let (AnyNode ptr) = Scope_node.get g scope name in
            let ptr = Node.as_data_exn ptr in

            let (AnyData index) =
                do_expr g idx_expr scope parent_fun cur_ret_node linker |> Option.value_exn
            in
            let field_ptr = Mem_nodes.create_addr_of_field ?parent_fun g loc ptr ~index "[]" in
            let el_typ =
                match field_ptr.typ with
                | Ptr p -> p
                | _ -> assert false
            in
            let load = Mem_nodes.create_load ?parent_fun g loc ~mem ~ptr:field_ptr "[]" in
            load.min_typ <- Some el_typ;
            Some (AnyData load))
    | Ast.VarAssign (name, expr) ->
        let (AnyData n) = do_expr g expr scope parent_fun cur_ret_node linker |> Option.value_exn in
        Scope_node.assign g scope name n;
        Some (AnyData n)
    | Ast.ArrayVarAssign (name, index, value) ->
        let (AnyData index) =
            do_expr g index scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData value) =
            do_expr g value scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyNode ptr) = Scope_node.get g scope name in
        let ptr = Node.as_data_exn ptr in
        let (AnyMem mem) = Scope_node.get_mem g scope in
        let field_ptr = Mem_nodes.create_addr_of_field ?parent_fun g loc ptr ~index "[]" in
        let el_typ =
            match field_ptr.typ with
            | Ptr p -> p
            | _ -> assert false
        in
        value.min_typ <- Some el_typ;
        let store_mem = Mem_nodes.create_store ?parent_fun g loc ~mem ~ptr:field_ptr "[]" ~value in
        Scope_node.set_mem g scope store_mem;
        Some (AnyData value)
    | Ast.Add (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Arithmetic_nodes.create_add g loc ?parent_fun lhs rhs))
    | Ast.Sub (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Arithmetic_nodes.create_sub g loc ?parent_fun lhs rhs))
    | Ast.Mul (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Arithmetic_nodes.create_mul g loc ?parent_fun lhs rhs))
    | Ast.Div (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Arithmetic_nodes.create_div g loc ?parent_fun lhs rhs))
    | Ast.Lsh (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bitop_nodes.create_lsh g loc ?parent_fun lhs rhs))
    | Ast.Rsh (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bitop_nodes.create_rsh g loc ?parent_fun lhs rhs))
    | Ast.BAnd (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bitop_nodes.create_band g loc ?parent_fun lhs rhs))
    | Ast.BOr (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bitop_nodes.create_bor g loc ?parent_fun lhs rhs))
    | Ast.Eq (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bool_nodes.create_eq g loc ?parent_fun lhs rhs))
    | Ast.NEq (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bool_nodes.create_neq g loc ?parent_fun lhs rhs))
    | Ast.Lt (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bool_nodes.create_lt g loc ?parent_fun lhs rhs))
    | Ast.LEq (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bool_nodes.create_leq g loc ?parent_fun lhs rhs))
    | Ast.Gt (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bool_nodes.create_gt g loc ?parent_fun lhs rhs))
    | Ast.GEq (lhs, rhs) ->
        let (AnyData lhs) =
            do_expr g lhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyData rhs) =
            do_expr g rhs scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        Some (Node.AnyData (Bool_nodes.create_geq g loc ?parent_fun lhs rhs))
    | Ast.Block (statements, expr) ->
        Scope_node.push scope;
        List.iter statements ~f:(fun s ->
            do_statement g s scope parent_fun cur_ret_node linker |> ignore);
        let n =
            Option.value_map expr ~default:None ~f:(fun e ->
                do_expr g e scope parent_fun cur_ret_node linker)
        in
        Scope_node.pop g scope;
        n
    | Ast.IfElse (cond, body, else_body) -> (
        let (AnyData n_cond) =
            do_expr g cond scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
        let n_if = If_node.create ?parent_fun g loc ~ctrl ~pred:n_cond in
        let n_true = Proj_node.create_ctrl ?parent_fun g loc n_if 0 in
        let n_false = Proj_node.create_ctrl ?parent_fun g loc n_if 1 in
        let false_scope = Scope_node.dup g scope in
        Scope_node.set_ctrl g scope n_true;
        let body_true = do_expr g body scope parent_fun cur_ret_node linker in
        Scope_node.set_ctrl g false_scope n_false;
        match else_body with
        | None ->
            Scope_node.merge ?parent_fun g loc ~this:scope ~other:false_scope;
            None
        | Some else_body -> (
            let body_false = do_expr g else_body false_scope parent_fun cur_ret_node linker in
            Scope_node.merge ?parent_fun g loc ~this:scope ~other:false_scope;
            match (body_true, body_false) with
            | None, None -> None
            | Some body_true, Some body_false ->
                let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
                Some
                  (AnyData (Phi_node.create_data ?parent_fun g loc ctrl [ body_true; body_false ]))
            | _, _ -> failwith "The two branches must have the same type"))
    | Ast.FnCall (expr, args) ->
        let (AnyData fun_ptr) =
            do_expr g expr scope parent_fun cur_ret_node linker |> Option.value_exn
        in
        let args =
            List.map args ~f:(fun arg ->
                do_expr g arg scope parent_fun cur_ret_node linker |> Option.value_exn)
        in
        (* Big return type handling (hidden ret ptr) is now done in the
           Struct_returns pass after IR construction, so recursive functions
           work correctly *)
        let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
        let (AnyMem mem) = Scope_node.get_mem g scope in
        let call_node, call_end = Fun_node.add_call ?parent_fun g loc ~ctrl ~mem ~fun_ptr args in
        let ctrl = Proj_node.create_ctrl ?parent_fun g loc call_end 0 in
        Scope_node.set_ctrl g scope ctrl;
        let return_mem = Proj_node.create_mem ?parent_fun g loc call_end 1 in
        Scope_node.set_mem g scope return_mem;
        let return_val = Proj_node.create_data ?parent_fun g loc call_end 2 in
        Some (AnyData return_val)
    (* | Ast.Return ->  *)
    | Ast.FnDeclaration (typ, param_names, body) ->
        (* TODO: when return value is big the function body will allocate
           memory for the struct, fill it up then at the end we copy the data
           into the memory provided by the caller. This should be optimised
           (like NRVO/URVO in C++) *)
        let ret_type, param_types =
            match typ with
            | Ast.Fn (ret, params) -> (ret, params)
            | _ -> assert false
        in
        let ret_type = get_type g scope ret_type in
        let param_types = List.map param_types ~f:(get_type g scope) in
        let fun_ptr_type = Types.make_fun_ptr param_types ret_type in
        let param_names, param_types =
            match fun_ptr_type with
            | FunPtr (Value { params; ret; fun_indices = _ }) ->
                (* make_fun_ptr adds a hidden ret ptr param for struct returns,
                   so prepend the identifier to keep param_names in sync *)
                if List.length params = List.length param_names then
                  (param_names, params)
                else (
                  assert (Types.equal (List.hd_exn params) ret);
                  (Scope_node.ret_identifier :: param_names, params))
            | _ -> assert false
        in
        let fun_node, ret_node = Fun_node.create g loc fun_ptr_type in
        let fun_idx = Linker.define linker fun_node in
        fun_node.parent_fun <- Some fun_idx;

        (match fun_node.kind with
        | Ctrl (Function k) -> fun_node.kind <- Ctrl (Function { k with idx = fun_idx }));
        let fun_ptr = Const_node.create_fun_ptr ?parent_fun g loc fun_node fun_idx in
        Scope_node.push scope;
        let (AnyCtrl old_ctrl) = Scope_node.get_ctrl g scope in
        let (AnyMem old_mem) = Scope_node.get_mem g scope in
        Node.G.toggle_node_undying g old_ctrl;
        Node.G.toggle_node_undying g old_mem;
        Scope_node.set_ctrl g scope fun_node;
        let mem = Fun_node.create_mem_param ~parent_fun:fun_idx g loc fun_node in
        Scope_node.set_mem g scope mem;
        List.zip_exn param_types param_names
        |> List.iteri ~f:(fun i (ptype, pname) ->
            if String.equal pname Scope_node.ret_identifier then
              (* skip this as it will be added in Struct_returns pass *)
              ()
            else
              let param_node = Fun_node.create_param ~parent_fun:fun_idx g loc fun_node ptype i in
              param_node.min_typ <- Some ptype;
              Scope_node.define g scope pname param_node false);
        let (AnyData body_n) =
            do_expr g body scope (Some fun_idx) (Some ret_node) linker
            |> Option.value
                 ~default:
                   (AnyData (Const_node.create_from_type ~parent_fun:fun_idx g body.loc Types.Void))
        in
        body_n.min_typ <- Some ret_type;
        (* Big return type handling (copy into ret and return ptr) is now
           done in the Struct_returns pass after IR construction *)
        let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
        let (AnyMem mem) = Scope_node.get_mem g scope in
        Fun_node.add_return ~parent_fun:fun_idx g ret_node ~ctrl ~mem ~val_n:body_n;
        Scope_node.pop g scope;
        Scope_node.set_ctrl g scope old_ctrl;
        Scope_node.set_mem g scope old_mem;
        Node.G.toggle_node_undying g old_ctrl;
        Node.G.toggle_node_undying g old_mem;
        Some (AnyData fun_ptr)
    | Ast.ExternalFnDeclaration (typ, _, external_name) ->
        let ret_type, param_types =
            match typ with
            | Ast.Fn (ret, params) -> (ret, params)
            | _ -> assert false
        in
        let ret_type = get_type g scope ret_type in
        let fun_ptr_type =
            Types.make_fun_ptr (List.map param_types ~f:(get_type g scope)) ret_type
        in
        let param_types =
            match fun_ptr_type with
            | FunPtr (Value { params; ret; fun_indices = _ }) -> params
            | _ -> assert false
        in
        let fun_node, ret_node = Fun_node.create g loc fun_ptr_type in
        let fun_idx = Linker.define linker fun_node in
        fun_node.parent_fun <- Some fun_idx;
        Linker.set_name linker fun_idx external_name;
        (match fun_node.kind with
        | Ctrl (Function k) -> fun_node.kind <- Ctrl (Function { k with idx = fun_idx }));
        let fun_ptr = Const_node.create_fun_ptr ?parent_fun g loc fun_node fun_idx in
        (* TODO the extern node should probably also pretend to use the MEMORY
           since it could store stuff into the passed in ptr when return value
           doesnt fit into a single register *)
        let params =
            List.mapi param_types ~f:(fun i param_type ->
                let param_node =
                    Fun_node.create_param ~parent_fun:fun_idx g loc fun_node param_type i
                in
                param_node.min_typ <- Some param_type;
                param_node)
        in
        let mem = Fun_node.create_mem_param ~parent_fun:fun_idx g loc fun_node in
        let ret_val = Extern_node.create ~parent_fun:fun_idx g loc ret_type external_name params in
        ret_val.min_typ <- Some ret_type;

        Fun_node.add_return ~parent_fun:fun_idx g ret_node ~ctrl:fun_node ~mem ~val_n:ret_val;
        Some (AnyData fun_ptr)
    | TypeDeclaration t ->
        let typ = Types.of_ast_type t in
        let c = Const_node.create_from_type ?parent_fun g loc (Type (Value typ)) in
        Some (AnyData c)
    | TypeInstantiation (type_name, fields) ->
        let (AnyNode typ_constant) = Scope_node.get g scope type_name in
        let typ =
            match typ_constant.typ with
            | Type (Value t) -> t
            | _ -> assert false
        in
        let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
        let (AnyMem mem) = Scope_node.get_mem g scope in
        let type_size = Types.get_size typ in
        let size = Const_node.create_int ?parent_fun g loc type_size in
        let n = Mem_nodes.create_new ?parent_fun g loc ~ctrl ~mem ~size typ in
        let mem = Proj_node.create_mem ?parent_fun g loc n 0 in
        Scope_node.set_mem g scope mem;
        let struct_node = Proj_node.create_data ?parent_fun g loc n 1 in
        struct_node.min_typ <- Some typ;
        let trait_fields, field_names =
            match typ with
            | Struct (Value { name = _; fields }) ->
                List.map fields ~f:fst
                |> List.partition_tf ~f:(fun s -> String.is_prefix s ~prefix:"$")
                |> Tuple2.map ~f:String.Set.of_list
            | _ ->
                (* TODO: this should also be allowed for primitives (e.g. i64{5}) *)
                assert false
        in
        let remaining_field_names =
            List.fold fields ~init:field_names ~f:(fun remaining_field_names (name, e) ->
                match name with
                | None -> failwith "todo anonymous field init"
                | Some name ->
                    if not (Set.mem remaining_field_names name) then
                      if Set.mem field_names name then
                        failwithf "%s:%d: Field %s is initialised multiple times" loc.filename
                          loc.line name ()
                      else
                        failwithf "%s:%d: Field %s is not part of type %s" loc.filename loc.line
                          name type_name ()
                    else
                      let (AnyData value) =
                          do_expr g e scope parent_fun cur_ret_node linker |> Option.value_exn
                      in
                      let field_type = Types.get_field_type typ name |> Option.value_exn in
                      value.min_typ <- Some field_type;
                      let dst = Mem_nodes.create_addr_of_field ?parent_fun g loc struct_node name in
                      let (AnyMem mem) = Scope_node.get_mem g scope in
                      let (AnyMem mem) =
                          match field_type with
                          | Struct _ ->
                              let src = Mem_nodes.create_addr_of ?parent_fun g loc value in
                              Node.AnyMem (Mem_nodes.create_copy ?parent_fun g loc ~mem ~src ~dst)
                          | _ ->
                              AnyMem
                                (Mem_nodes.create_store ?parent_fun g loc ~mem ~ptr:dst name ~value)
                      in
                      Scope_node.set_mem g scope mem;
                      Set.remove remaining_field_names name)
        in
        (* trait fields are just functions. They are already checked in the TraitImplementation expression *)
        let remaining_field_names = Set.diff remaining_field_names trait_fields in
        if not (Set.is_empty remaining_field_names) then
          failwithf "%s:%d: Following fields were not initialised: %s" loc.filename loc.line
            (Set.to_list remaining_field_names |> String.concat ~sep:", ")
            ();
        Some (AnyData struct_node)
    | FieldAccess (e, field_name) -> (
        let (AnyData base) = do_expr g e scope parent_fun cur_ret_node linker |> Option.value_exn in
        let (AnyMem mem) = Scope_node.get_mem g scope in
        match base.typ with
        | Trait (Value t) ->
            let field_name = "$" ^ t.name ^ "$" ^ field_name in
            let field_ptr = Mem_nodes.create_addr_of_field ?parent_fun g loc base field_name in
            let load = Mem_nodes.create_load ?parent_fun g loc ~mem ~ptr:field_ptr field_name in
            Some (AnyData load)
        | Type (Value t) ->
            let base_name =
                match t with
                | Struct (Value { name; fields = _ }) -> name
                | Trait (Value { name; fields = _ }) -> name
                | _ -> assert false
            in
            let field_name =
                match t with
                | Struct (Value { name = _; fields }) ->
                    List.find_map_exn fields ~f:(fun (name, t) ->
                        (* mangled names are like $trait_name$fun_name so we check suffix *)
                        (* TODO: name collisions *)
                        if String.is_suffix name ~suffix:field_name then
                          Some name
                        else
                          None)
                | Trait (Value { name = _; fields }) -> field_name
                | _ -> assert false
            in
            let field_typ = Types.get_field_type t field_name |> Option.value ~default:ALL in
            (* TODO: only functions are accessible on Types for now *)
            assert (Types.is_a field_typ (FunPtr All));
            let (AnyNode n) = Scope_node.get g scope (base_name ^ field_name) in
            let n = Node.as_data_exn n in
            Some (AnyData n)
        | _ ->
            let field_ptr = Mem_nodes.create_addr_of_field ?parent_fun g loc base field_name in
            let load = Mem_nodes.create_load ?parent_fun g loc ~mem ~ptr:field_ptr field_name in
            Some (AnyData load))
    | TraitDeclaration funs ->
        let funs =
            List.map funs ~f:(fun (n, t) ->
                let t = Types.of_ast_type t in
                assert (Types.is_a t (FunPtr All));
                (n, t))
        in
        let t = Types.make_trait "" funs in
        let c = Const_node.create_from_type ?parent_fun g loc (Type (Value t)) in
        Some (AnyData c)
    | _ -> failwithf "TODO: unimplemented %s" (Ast.show_expr e.node) ()

let define_builtin_type g scope t =
    let n =
        Const_node.create_from_type g
          { filename = ""; line = 0; col = 0 }
          (Type (Value (Types.of_ast_type (Ast.Type t))))
    in
    Scope_node.define g scope t n true

let of_ast ast linker =
    let loc : Ast.loc = { filename = ""; line = 0; col = 0 } in
    let start = Start_node.create loc in
    let stop = Stop_node.create loc in
    let g = Node.G.create ~start:(AnyNode start) ~stop:(AnyNode stop) in
    let ctrl = Proj_node.create_ctrl g loc start 0 in
    let mem = Proj_node.create_mem g loc start 1 in
    let scope = Scope_node.create g in
    Scope_node.set_ctrl g scope ctrl;
    Scope_node.set_mem g scope mem;
    let builtin_types =
        [ "bool"; "str"; "void" ]
        @ List.init 128 ~f:(fun i ->
            if i < 64 then Printf.sprintf "i%d" (i + 1) else Printf.sprintf "u%d" (i - 64 + 1))
    in
    List.iter builtin_types ~f:(define_builtin_type g scope);
    Core.List.iter ast ~f:(fun s -> do_statement g s scope None None linker);
    let (AnyCtrl ctrl) = Scope_node.get_ctrl g scope in
    Node.G.set_ctrl g stop ctrl;
    Scope_node.set_ctrl g scope stop;
    Node.G.set_node_inputs g stop { mem = Some (Scope_node.get_mem g scope) };
    (* makes life easier and we dont need the scope anymore i think*)
    Node.G.remove_node g scope;
    (* Node.G.add_dependencies g stop (Node.G.get_dependencies g scope |> List.tl_exn); *)
    Node.G.get_dependants g start
    |> List.iter ~f:(fun (AnyNode n) ->
        if Node.G.get_dependants g n |> List.is_empty then Node.G.remove_node g n);
    g
