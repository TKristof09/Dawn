open Core

let rev_post_order_cfg g node =
    let visited = Hash_set.create (module Machine_node) in
    Hash_set.add visited node;

    let rec dfs (node : Machine_node.t) acc =
        if Machine_node.is_control_node node then (
          Hash_set.add visited node;

          let new_acc =
              (* Use fold right to get the true branch of if statements first, this helps put the loop body before the loop exit *)
              Graph.get_dependants g node
              |> List.filter ~f:Machine_node.is_control_node
              |> List.fold_right
                   ~f:(fun dep current_acc ->
                     if not (Hash_set.mem visited dep) then
                       dfs dep current_acc
                     else
                       current_acc)
                   ~init:acc
          in

          node :: new_acc)
        else
          acc
    in
    dfs node []

(* consider caching this *)
let rec idepth g (node : Machine_node.t) =
    match node.kind with
    | Ideal Start -> 0
    | FunctionProlog _ -> 0
    | Ideal Region ->
        Graph.get_dependencies g node
        |> List.fold ~init:0 ~f:(fun acc n ->
            match n with
            | Some n -> max acc (idepth g n)
            | None -> acc)
    | Ideal Loop -> 1 + idepth g (Loop_node.get_entry_edge g node)
    | _ when Machine_node.is_control_node node ->
        1 + idepth g (Graph.get_dependency g node 0 |> Option.value_exn)
    | _ -> idepth g (Graph.get_dependency g node 0 |> Option.value_exn)

(* consider caching this *)
let rec loop_depth g (n : Machine_node.t) =
    match n.kind with
    | Ideal Loop ->
        let entry_depth = Loop_node.get_entry_edge g n |> loop_depth g in
        entry_depth + 1
    | Ideal Start -> 1
    | FunctionProlog _ -> 1
    | Ideal Region -> Graph.get_dependency g n 1 |> Option.value_exn |> loop_depth g
    | Ideal (CProj 1) -> (
        let p = Graph.get_dependency g n 0 |> Option.value_exn in
        match p.kind with
        | Jmp _ -> (
            let p = Graph.get_dependency g p 0 |> Option.value_exn in
            match p.kind with
            | Ideal Loop -> Loop_node.get_entry_edge g p |> loop_depth g
            | _ -> Graph.get_dependency g n 0 |> Option.value_exn |> loop_depth g)
        | _ -> Graph.get_dependency g n 0 |> Option.value_exn |> loop_depth g)
    | _ -> Graph.get_dependency g n 0 |> Option.value_exn |> loop_depth g

let rec dom g (n : Machine_node.t) =
    match n.kind with
    | Ideal Start -> assert false
    | Ideal Loop -> Loop_node.get_entry_edge g n
    | Ideal Region
    | FunctionProlog _ ->
        Graph.get_dependencies g n |> List.filter_opt |> List.reduce_exn ~f:(common_dom g)
    | _ -> Graph.get_dependency g n 0 |> Option.value_exn

and common_dom g (lhs : Machine_node.t) (rhs : Machine_node.t) =
    if Machine_node.equal lhs rhs then
      lhs
    else
      let comp = idepth g lhs - idepth g rhs in
      if comp >= 0 then
        common_dom g (dom g lhs) rhs
      else
        common_dom g lhs (dom g rhs)

let get_function g n =
    let rec aux (cur : Machine_node.t) =
        match cur.kind with
        | FunctionProlog i -> i
        | Ideal Start -> 0
        | _ ->
            let p =
                (* TODO: this hack is so bad, dom should just not throw exceptions... *)
                match Graph.get_dependency g cur 0 with
                | None -> (
                    if Machine_node.is_control_node cur then
                      dom g cur
                    else
                      let non_const =
                          Graph.get_dependencies g cur
                          |> List.find ~f:(function
                            | None -> false
                            | Some n -> (
                                match n.kind with
                                | Int _ -> false
                                | _ -> true))
                      in
                      match non_const with
                      | Some p -> Option.value_exn p
                      | None -> Graph.get_dependencies g cur |> List.filter_opt |> List.hd_exn)
                | Some _ -> dom g cur
            in
            aux p
    in
    aux n

let schedule_early g =
    let already_scheduled = Hash_set.create ~size:(Graph.get_num_nodes g) (module Machine_node) in
    let start =
        Graph.find g ~f:(fun (n : Machine_node.t) ->
            match n.kind with
            | FunctionProlog _ -> true
            | _ -> false)
        |> Option.value ~default:(Graph.get_start g)
    in
    let rec schedule (node : Machine_node.t) =
        assert (not (Machine_node.is_control_node node));
        if not (Hash_set.mem already_scheduled node) then (
          Hash_set.add already_scheduled node;
          let deps = Graph.get_dependencies g node in
          List.filter_map deps ~f:(function
            | None -> None
            | Some n -> (
                match n.kind with
                | Ideal Phi
                | Param _
                | _
                  when Machine_node.is_control_node n ->
                    None
                | _ -> Some n))
          |> List.iter ~f:schedule;
          let is_pinned =
              match node.kind with
              | Ideal Phi
              | DProj _ ->
                  true
              | _ -> false
          in
          if not is_pinned then
            let scheduled_n, _ =
                List.fold
                  (List.tl deps |> Option.value ~default:[])
                  ~init:(start, idepth g start)
                  ~f:(fun (max_n, max_depth) n ->
                    match n with
                    | None -> (max_n, max_depth)
                    | Some n ->
                        let cfg = Graph.get_dependency g n 0 |> Option.value_exn in
                        let d = idepth g cfg in
                        if d > max_depth then
                          (cfg, d)
                        else
                          (max_n, max_depth))
            in
            Graph.set_dependency g node (Some scheduled_n) 0)
    in
    rev_post_order_cfg g start
    |> List.iter ~f:(fun n ->
        assert (Machine_node.is_control_node n);
        Graph.get_dependencies g n
        |> List.filter_opt
        |> List.iter ~f:(fun n ->
            match n.kind with
            | _ when Machine_node.is_control_node n -> ()
            | _ -> schedule n);
        match n.kind with
        | Ideal Region
        | Ideal Loop ->
            Graph.get_dependants g n
            |> List.filter ~f:(fun n ->
                match n.kind with
                | Ideal Phi -> true
                | _ -> false)
            |> List.iter ~f:schedule
        | _ -> ())

let schedule_late g =
    let m = Hashtbl.create ~size:(Graph.get_num_nodes g) (module Machine_node) in
    let is_forward_edge (node : Machine_node.t) (dependant : Machine_node.t) =
        match dependant.kind with
        | Ideal Loop -> not (Loop_node.get_back_edge g dependant |> Machine_node.equal node)
        | Ideal Phi -> (
            match (Graph.get_dependency g dependant 0 |> Option.value_exn).kind with
            | Ideal Loop -> not (Loop_node.get_back_edge g dependant |> Machine_node.equal node)
            | _ -> true)
        | _ -> true
    in
    let get_block (node : Machine_node.t) (dependant : Machine_node.t) =
        match dependant.kind with
        | Ideal Phi ->
            let idx =
                Stdlib.List.find_index
                  (function
                    | None -> false
                    | Some n -> Machine_node.equal n node)
                  (Graph.get_dependencies g dependant)
                |> Option.value_exn
            in
            let region = Graph.get_dependency g dependant 0 |> Option.value_exn in
            Graph.get_dependency g region idx |> Option.value_exn
        | _ -> Hashtbl.find_exn m dependant
    in

    let find_best early late =
        let rec get_path cur =
            if Machine_node.equal cur early then
              [ early ]
            else
              cur :: get_path (dom g cur)
        in
        let is_better best cur =
            loop_depth g cur < loop_depth g best
            || idepth g cur > idepth g best
            ||
            match best.kind with
            | Jmp _ -> true
            | _ -> false
        in
        print_endline "";
        let best =
            List.reduce_exn (get_path late) ~f:(fun best n -> if is_better best n then n else best)
        in
        assert (Machine_node.is_blockhead best);
        best
    in
    let rec schedule (node : Machine_node.t) =
        if not (Hashtbl.mem m node) then (
          (match node.kind with
          | Ideal Phi ->
              Hashtbl.set m ~key:node ~data:(Graph.get_dependency g node 0 |> Option.value_exn)
          | DProj _ ->
              let cfg = Graph.get_dependency g node 0 |> Option.value_exn in
              if Machine_node.is_control_node cfg then
                Hashtbl.set m ~key:node ~data:cfg
              else
                ()
          | _ when Machine_node.is_control_node node ->
              if Machine_node.is_blockhead node then
                Hashtbl.set m ~key:node ~data:node
              else
                Hashtbl.set m ~key:node ~data:(Graph.get_dependency g node 0 |> Option.value_exn)
          | _ -> ());

          List.iter (Graph.get_dependants g node) ~f:(fun n ->
              if is_forward_edge node n then schedule n);

          if not (Hashtbl.mem m node) then
            let lca =
                List.map (Graph.get_dependants g node) ~f:(get_block node)
                |> List.reduce_exn ~f:(common_dom g)
            in
            let early = Graph.get_dependency g node 0 |> Option.value_exn in
            let early =
                if Machine_node.is_control_node early then
                  early
                else
                  Graph.get_dependency g early 0 |> Option.value_exn
            in
            (* if node.id = 73 then ( *)
            (*   Printf.printf "EARLY: %s LATE: %s\n" (Machine_node.show early) (Machine_node.show lca); *)
            (*   assert false); *)
            let best = find_best early lca in
            Hashtbl.set m ~key:node ~data:best)
    in
    schedule (Graph.get_start g);
    Hashtbl.iteri m ~f:(fun ~key ~data ->
        match key.kind with
        | Ideal Phi -> ()
        | _ when Machine_node.is_control_node key -> ()
        | DProj _ -> ()
        | _ -> Graph.set_dependency g key (Some data) 0)

let score g (n : Machine_node.t) =
    match n.kind with
    | DProj _
    | Ideal (CProj 0) ->
        1001
    | Ideal (CProj 1) -> 1002
    | Ideal Phi
    | Param _ ->
        1000
    | _ when Machine_node.is_control_node n -> 1
    | Ideal _ -> 500
    | _ ->
        (* this makes things like cmp nodes get low prio so they get put at the end of the block right before the jump so the flags don't get overwritten inbetween*)
        if Graph.get_dependants g n |> List.exists ~f:Machine_node.is_control_node then
          10
        else
          500

let schedule_flat g =
    let schedule_main nodes =
        let scheduled = Dynarray.create () in
        let not_ready = Hash_set.of_list (module Machine_node) nodes in
        List.iter nodes ~f:(fun n ->
            if Machine_node.is_multi_output n then
              Graph.get_dependants g n
              |> List.iter ~f:(fun n ->
                  match n.kind with
                  | DProj _ -> Hash_set.add not_ready n
                  | _ -> ()));
        let ready =
            Hash_set.filter not_ready ~f:(fun n ->
                let deps =
                    Graph.get_dependencies g n
                    |> List.filter_opt
                    |> Hash_set.of_list (module Machine_node)
                in
                Hash_set.for_all deps ~f:(fun x -> not (Hash_set.mem not_ready x)))
        in
        let not_ready = Hash_set.diff not_ready ready in
        let is_ready node =
            List.for_all
              (Graph.get_dependencies g node |> List.tl_exn)
              ~f:(function
                | None -> true
                | Some n -> (not (Hash_set.mem ready n)) && not (Hash_set.mem not_ready n))
        in
        while not (Hash_set.is_empty ready && Hash_set.is_empty not_ready) do
          let best =
              Hash_set.max_elt ready ~compare:(fun a b -> Int.compare (score g a) (score g b))
              |> Option.value_exn (* ready should never be empty if not_ready isnt empty *)
          in
          Dynarray.add_last scheduled best;
          Hash_set.remove ready best;
          List.iter (Graph.get_dependants g best) ~f:(fun n ->
              if Hash_set.mem not_ready n && is_ready n then (
                Hash_set.add ready n;
                Hash_set.remove not_ready n))
        done;
        Dynarray.to_list scheduled
    in
    let cfg = rev_post_order_cfg g (Graph.get_start g) in
    cfg
    |> List.filter ~f:Machine_node.is_blockhead
    |> List.map ~f:(fun bb ->
        bb :: (Graph.get_dependants g bb |> List.filter ~f:(Fun.negate Machine_node.is_blockhead))
        |> schedule_main)

let duplicate_constants g function_graphs =
    let start = Graph.get_start g in
    let consts =
        Graph.get_dependants g start
        |> List.filter ~f:(fun (n : Machine_node.t) ->
            match n.kind with
            | Int _ -> true
            | _ -> false)
    in
    let new_copies = Hashtbl.create (module Int) in
    List.iter consts ~f:(fun n ->
        Graph.get_dependants g n
        |> List.filter ~f:(fun dep ->
            match dep.kind with
            | Param _ -> false
            | _ -> true)
        |> List.iter ~f:(fun dep ->
            let fun_idx = get_function g dep in
            if fun_idx <> 0 then
              let dep_idx, _ =
                  List.findi_exn (Graph.get_dependencies g dep) ~f:(fun _ dep ->
                      match dep with
                      | None -> false
                      | Some dep -> Machine_node.equal dep n)
              in
              let tbl =
                  Hashtbl.find_or_add new_copies fun_idx ~default:(fun _ ->
                      Hashtbl.create (module Machine_node))
              in
              Hashtbl.add_multi tbl ~key:n ~data:(dep, dep_idx)));
    List.iteri function_graphs ~f:(fun fun_idx g ->
        match Hashtbl.find new_copies fun_idx with
        | None -> ()
        | Some tbl ->
            Hashtbl.iteri tbl ~f:(fun ~key ~data ->
                let new_copy = { key with id = Machine_node.next_id () } in
                List.iter data ~f:(fun (dep, dep_idx) ->
                    Graph.set_dependency g dep (Some new_copy) dep_idx);
                Graph.add_dependencies g new_copy [ Some (Graph.get_start g) ]))

let schedule g =
    let g = Machine_node.convert_graph g in
    (* Ir_printer.to_dot_machine g |> print_endline; *)
    let per_function_graphs = Graph.partition g ~f:(get_function g) in
    (* connect return nodes to stop node *)
    (* TODO: we might want separate stop and start nodes per graph if it messes
       up reg alloc or other downstream usage that looks up the neighbours of
       these nodes *)
    duplicate_constants g per_function_graphs;
    (* cleanup useless const nodes in the first function graph (the "top level"
       one). This happens if a constant is only used in a function because in
       that case it is still left in the top-level graph at first and then copied into
       the function's graph so the one in the top-level is left without any dependants
    *)
    let top_level = List.hd_exn per_function_graphs in
    Graph.get_dependants top_level (Graph.get_start top_level)
    |> List.iter ~f:(fun n ->
        if Graph.get_dependants top_level n |> List.is_empty then Graph.remove_node top_level n);

    List.iter per_function_graphs ~f:(fun g ->
        let ret_node =
            Graph.find g ~f:(fun n ->
                match n.kind with
                | Return -> true
                | _ -> false)
        in
        match ret_node with
        | None -> ()
        | Some ret_node -> Graph.add_dependencies g (Graph.get_stop g) [ Some ret_node ]);
    (* FIXME schedule early pulls out nodes from branches of an if to before the if. They then get pulled back in to the branch in schedule_flat but it still feels wrong for schedule_early to be able to pull them out *)
    List.map per_function_graphs ~f:(fun g ->
        schedule_early g;
        schedule_late g;
        (* Ir_printer.to_dot_machine g |> print_endline; *)
        (g, schedule_flat g))
