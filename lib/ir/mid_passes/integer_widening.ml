open Core

(* This pass goes over integer operations and casts integers where necessary.
   We only do automatic casting when the desired integer type has larger bit
   width as the current operands. This is to ensure that e.g. arithmetic nodes
   have the same width inputs or that a function argument is the same integer
   size as the parameter .

   This cast will get translated to either zero extension or sign extension in
   the backend IR *)

let do_node : type a b. Node.G.readwrite Node.G.t -> (a, b) Node.t -> unit =
   fun g n ->
    let create_cast : type a c d.
        Node.G.readwrite Node.G.t ->
        (a, Node.data) Node.t ->
        (c, d) Node.t ->
        Ast.loc ->
        int ->
        Node.any_data =
       fun g def use loc fixed_width ->
        [%log.debug "Widening %a to %a to width %d" Node.pp def Node.pp use fixed_width];
        (* This function is only for casting integers into correct bit width *)
        let def_int_t =
            match def.typ with
            | Integer (Value v) -> v
            | _ -> assert false
        in
        match def.kind with
        | Data Constant ->
            let v = Types.get_integer_const_exn def.typ in
            let t = Types.make_int_const ~fixed_width v in
            Node.AnyData (Const_node.create_from_type ?parent_fun:def.parent_fun g loc t)
        | _ -> (
            let cast_typ =
                Types.make_int ~num_widens:def_int_t.num_widens ~fixed_width def_int_t.min
                  def_int_t.max
            in
            let has_cast =
                Node.G.get_dependants g def
                |> List.find_map ~f:(fun (AnyNode n) ->
                    match n.kind with
                    | Data Cast ->
                        if Types.equal n.typ cast_typ then Some (Node.AnyData n) else None
                    | _ -> None)
            in
            match has_cast with
            | None ->
                let cast_node = Node.create_data ?parent_fun:def.parent_fun loc cast_typ Cast in
                (* TODO: should we use the def's control input or the use's or just put none and let the scheduler figure it out? *)
                Node.G.add_node g cast_node { Node.input = Some (AnyData def) };
                (match Node.G.get_ctrl g def with
                | None -> ()
                | Some (AnyNode ctrl) -> Node.G.set_ctrl g cast_node ctrl);
                AnyData cast_node
            | Some cast_node -> cast_node)
    in
    let n_fixed_width =
        match n.typ with
        | Integer (Value { min; max; num_widens; fixed_width }) -> fixed_width
        | _ -> None
    in
    let as_binop : type a b. (a, b) Node.t -> (Node.binop, Node.data) Node.t =
       fun n ->
        match n.kind with
        | Data Add -> n
        | Data Sub -> n
        | Data Div -> n
        | Data Mul -> n
        | Data Eq -> n
        | Data NEq -> n
        | Data Lt -> n
        | Data LEq -> n
        | Data Gt -> n
        | Data GEq -> n
        | Data BAnd -> n
        | Data BOr -> n
        | _ -> assert false
    in
    match n.Node.kind with
    | Data Add
    | Data Sub
    | Data Div ->
        let n = as_binop n in
        let n_size = Types.get_size n.typ in
        let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let lhs_size = Types.get_size lhs.typ in
        let rhs_size = Types.get_size rhs.typ in
        (* Lowering bit width requires explicit cast in the source code. Only
           widening is done automatically. If no fixed width for n then all good. *)
        (* TODO: some nice error message*)
        if Option.is_some n_fixed_width then
          assert (n_size >= lhs_size && n_size >= rhs_size);
        let size = max n_size (max lhs_size rhs_size) in
        (* Don't need to do case if n_size < size since this happens only if n
           has no fixed_width, in which case the width will be calculated from
           the range anyway *)
        let lhs_casted =
            if lhs_size < size then
              create_cast g lhs n n.loc (size * 8)
            else
              AnyData lhs
        in
        let rhs_casted =
            if rhs_size < size then
              create_cast g rhs n n.loc (size * 8)
            else
              AnyData rhs
        in
        Node.G.set_node_inputs g n { Node.lhs = Some lhs_casted; rhs = Some rhs_casted }
    | Data Mul ->
        let n = as_binop n in
        let n_size = Types.get_size n.typ in
        let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let lhs_size = Types.get_size lhs.typ in
        let rhs_size = Types.get_size rhs.typ in
        (* Lowering bit width requires explicit cast in the source code. Only
           widening is done automatically. If no fixed width for n then all good. *)
        assert (Option.is_none n_fixed_width || (n_size >= lhs_size && n_size >= rhs_size));
        (* Mul needs at least 16bit inputs *)
        let n_size = max 2 n_size in
        let size = max n_size (max lhs_size rhs_size) in
        (* Don't need to do case if n_size < size since this happens only if n
           has no fixed_width, in which case the width will be calculated from
           the range anyway *)
        let lhs_casted =
            if lhs_size < size then
              create_cast g lhs n n.loc (size * 8)
            else
              AnyData lhs
        in
        let rhs_casted =
            if rhs_size < size then
              create_cast g rhs n n.loc (size * 8)
            else
              AnyData rhs
        in
        Node.G.set_node_inputs g n { Node.lhs = Some lhs_casted; rhs = Some rhs_casted }
    | Data Eq
    | Data NEq
    | Data Lt
    | Data LEq
    | Data Gt
    | Data GEq
    | Data BAnd
    | Data BOr ->
        let n = as_binop n in
        let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let lhs_size = Types.get_size lhs.typ in
        let rhs_size = Types.get_size rhs.typ in
        if lhs_size < rhs_size then
          let lhs = create_cast g lhs n n.loc (rhs_size * 8) in
          Node.G.set_node_inputs g n { Node.lhs = Some lhs; rhs = Some (AnyData rhs) }
        else if lhs_size > rhs_size then
          let rhs = create_cast g rhs n n.loc (lhs_size * 8) in
          Node.G.set_node_inputs g n { Node.lhs = Some (AnyData lhs); rhs = Some rhs }
    | Ctrl FunctionCall ->
        (* FunctionCall does the arg <-> param casting because it has easier access to all the needed nodes. *)
        let n = Node.unpack_exn n (Ctrl FunctionCall) in
        let { Node.fun_ptr; mem; args } = Node.G.get_dependencies_exn g n in
        let functions =
            Node.G.get_dependants g n
            |> List.filter_map ~f:(fun (AnyNode n) : (Node.fun_def, Node.ctrl) Node.t option ->
                match n.kind with
                | Ctrl (Function _) -> Some n
                | _ -> None)
        in
        let already_casted = Hash_set.create (module Node.AnyData) in
        let new_args =
            List.fold functions ~init:args ~f:(fun args f ->
                let _, params = Fun_node.get_param_nodes (Node.G.readonly g) f in
                List.zip_exn params args
                |> List.map ~f:(fun (param, arg) ->
                    match arg with
                    | None -> assert false
                    | Some (AnyData arg) ->
                        assert (Types.is_a arg.typ param.typ);

                        let param_size = Types.get_size param.typ in
                        let arg_size = Types.get_size arg.typ in
                        if arg_size < param_size then (
                          let cast = create_cast g arg param n.loc (param_size * 8) in
                          let { Node.phi_inputs } = Node.G.get_dependencies_exn g param in
                          let param_inputs =
                              List.map phi_inputs ~f:(function
                                | None -> None
                                | Some (AnyData n) ->
                                    if Node.equal n arg then Some cast else Some (AnyData n))
                          in
                          Node.G.set_node_inputs g param { Node.phi_inputs = param_inputs };
                          if not (Hash_set.mem already_casted (Node.AnyData arg)) then (
                            Hash_set.add already_casted (AnyData arg);
                            Some (create_cast g arg n n.loc (param_size * 8)))
                          else
                            Some (Node.AnyData arg))
                        else
                          Some (Node.AnyData arg)))
        in
        Node.G.set_node_inputs g n { Node.fun_ptr; mem; args = new_args }
    | Data (AddrOfField f) -> (
        let n = Node.unpack_exn n (Data (AddrOfField f)) in
        let { Node.place; offset } = Node.G.get_dependencies_exn g n in
        match offset with
        | None -> ()
        | Some (AnyData offs) ->
            let offs_size = Types.get_size offs.typ in
            if offs_size < 8 then
              let cast = create_cast g offs n n.loc 64 in
              Node.G.set_node_inputs g n { Node.place; offset = Some cast })
    | Mem New -> (
        let n = Node.unpack_exn n (Mem New) in
        let { Node.mem; size } = Node.G.get_dependencies_exn g n in
        match size with
        | None -> assert false
        | Some (AnyData size) ->
            let size_size = Types.get_size size.typ in
            if size_size < 8 then
              let cast = create_cast g size n n.loc 64 in
              Node.G.set_node_inputs g n { Node.mem; size = Some cast })
    | Mem (Store f) ->
        let { Node.mem; ptr; value } = Node.G.get_dependencies_exn g n in
        let (AnyData value) = Option.value_exn value in
        let (AnyData ptr_unwrapped) = Option.value_exn ptr in
        if Types.is_a value.typ (Integer All) then
          let value_size = Types.get_size value.typ in
          let field_type =
              match ptr_unwrapped.typ with
              | Ptr p -> p
              | _ -> assert false
          in
          let field_size = Types.get_size field_type in
          if value_size < field_size then
            let casted = create_cast g value n n.loc (field_size * 8) in
            Node.G.set_node_inputs g n { mem; ptr; value = Some casted }
    | _ -> ()

let run g =
    let nodes =
        Node.G.fold g ~init:[] ~f:(fun acc (AnyNode n) ->
            if Types.is_high n.Node.typ then
              (* HACK: skip Any typed nodes, these are leftovers from unreachable functions in SCCP. Once i put in deadcode elim they should no longer be an issue *)
              acc
            else
              Node.AnyNode n :: acc)
    in
    List.iter nodes ~f:(fun (AnyNode n) -> do_node g n)
