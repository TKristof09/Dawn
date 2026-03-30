open Core

let do_node g (n : Node.t) =
    let create_cast g (def : Node.t) (use : Node.t) loc fixed_width =
        [%log.debug "Widening %a to %a to width %d" Node.pp def Node.pp use fixed_width];
        (* This function is only for casting integers into correct bit width *)
        let def_int_t =
            match def.typ with
            | Integer (Value v) -> v
            | _ -> assert false
        in

        let dep_idx, _ =
            List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                match dep with
                | None -> false
                | Some dep -> Node.equal dep def)
        in
        match def.kind with
        | Data Constant ->
            let v = Types.get_integer_const_exn def.typ in
            let t = Types.make_int_const ~fixed_width v in
            let n = Const_node.create_from_type ?parent_fun:def.parent_fun g loc t in
            Graph.set_dependency g use (Some n) dep_idx
        | _ -> (
            let cast_typ =
                Types.make_int ~num_widens:def_int_t.num_widens ~fixed_width def_int_t.min
                  def_int_t.max
            in
            let has_cast =
                Graph.get_dependants g def
                |> List.find ~f:(fun n ->
                    Poly.equal n.kind (Data Cast) && Types.equal n.typ cast_typ)
            in
            match has_cast with
            | None ->
                let cast_node = Node.create_data ?parent_fun:def.parent_fun loc cast_typ Cast in
                (* TODO: should we use the def's control input or the use's or just put none and let the scheduler figure it out? *)
                Graph.add_dependencies g cast_node [ Graph.get_dependency g def 0; Some def ];
                Graph.set_dependency g use (Some cast_node) dep_idx
            | Some cast_node -> Graph.set_dependency g use (Some cast_node) dep_idx)
    in
    let n_fixed_width =
        match n.typ with
        | Integer (Value { min; max; num_widens; fixed_width }) -> fixed_width
        | _ -> None
    in
    match n.kind with
    | Data Add
    | Data Sub
    | Data Div ->
        let n_size = Types.get_size n.typ in
        let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
        let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
        let lhs_size = Types.get_size lhs.typ in
        let rhs_size = Types.get_size rhs.typ in
        (* Lowering bit width requires explicit cast in the source code. Only widening is done automatically. *)
        (* TODO: some nice error message*)
        if Option.is_some n_fixed_width then
          assert (n_size >= lhs_size && n_size >= rhs_size);
        if lhs_size < n_size then
          create_cast g lhs n n.loc (n_size * 8);
        if rhs_size < n_size then
          create_cast g rhs n n.loc (n_size * 8)
    | Data Mul ->
        let n_size = Types.get_size n.typ in
        let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
        let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
        let lhs_size = Types.get_size lhs.typ in
        let rhs_size = Types.get_size rhs.typ in
        (* Lowering bit width requires explicit cast in the source code. Only widening is done automatically. *)
        assert (n_size >= lhs_size && n_size >= rhs_size);
        (* Mul needs at least 16bit inputs *)
        let n_size = max 2 n_size in
        if lhs_size < n_size then
          create_cast g lhs n n.loc (n_size * 8);
        if rhs_size < n_size then
          create_cast g rhs n n.loc (n_size * 8)
    | Data Eq
    | Data NEq
    | Data Lt
    | Data LEq
    | Data Gt
    | Data GEq
    | Data BAnd
    | Data BOr ->
        let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
        let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
        let lhs_size = Types.get_size lhs.typ in
        let rhs_size = Types.get_size rhs.typ in
        if lhs_size < rhs_size then
          create_cast g lhs n n.loc (rhs_size * 8)
        else if lhs_size > rhs_size then
          create_cast g rhs n n.loc (lhs_size * 8)
    | Ctrl FunctionCall ->
        (* FunctionCall does the arg <-> param casting because it has easier access to all the needed nodes. *)
        let args = Graph.get_dependencies g n in
        let args = List.drop args 2 |> List.filter_opt in
        let functions =
            Graph.get_dependants g n
            |> List.filter ~f:(fun n ->
                match n.kind with
                | Ctrl (Function _) -> true
                | _ -> false)
        in
        let already_casted = Hash_set.create (module Node) in
        List.iter functions ~f:(fun f ->
            let params =
                Graph.get_dependants g f
                |> List.filter ~f:(fun n ->
                    match n.kind with
                    | Data (Param _) -> true
                    | _ -> false)
            in
            List.zip_exn params args
            |> List.iter ~f:(fun (param, arg) ->
                assert (Types.is_a arg.typ param.typ);
                if Types.equal Memory param.typ then
                  ()
                else
                  let param_size = Types.get_size param.typ in
                  let arg_size = Types.get_size arg.typ in
                  if arg_size < param_size then (
                    create_cast g arg param n.loc (param_size * 8);
                    if not (Hash_set.mem already_casted arg) then (
                      Hash_set.add already_casted arg;
                      create_cast g arg n n.loc (param_size * 8)))))
    | Mem (Store _)
    | Mem (Load _) ->
        let offs = Graph.get_dependency g n 3 |> Option.value_exn in
        let offs_size = Types.get_size offs.typ in
        if offs_size < 8 then
          create_cast g offs n n.loc 64
    | Mem New ->
        let size = Graph.get_dependency g n 2 |> Option.value_exn in
        let size_size = Types.get_size size.typ in
        if size_size < 8 then
          create_cast g size n n.loc 64
    | _ -> ()

let run g =
    let nodes =
        Graph.fold g ~init:[] ~f:(fun acc n ->
            if Types.is_high n.Node.typ then
              (* HACK: skip Any typed nodes, these are leftovers from unreachable functions in SCCP. Once i put in deadcode elim they should no longer be an issue *)
              acc
            else
              n :: acc)
    in
    List.iter nodes ~f:(do_node g);
    (* TODO: this is because Graph.set_dependency doesn't clean up nodes if they no longer have any uses *)
    Graph.cleanup g
