open Core

let rev_post_order_bfs_ctrl g node =
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
let rec loop_depth (g : Machine_node.t Graph.t) (n : Machine_node.t) =
    match n.kind with
    | Ideal Loop ->
        let entry_depth = Loop_node.get_entry_edge g n |> loop_depth g in
        entry_depth + 1
    | Ideal Start -> 1
    | Ideal Region -> Graph.get_dependency g n 1 |> Option.value_exn |> loop_depth g
    | _ -> Graph.get_dependency g n 0 |> Option.value_exn |> loop_depth g

let schedule_early (g : Machine_node.t Graph.t) =
    let already_scheduled = Hash_set.create ~size:(Graph.get_num_nodes g) (module Machine_node) in
    let rec schedule (node : Machine_node.t) =
        if
          (not (Hash_set.mem already_scheduled node))
          && (not (Machine_node.is_control_node node))
          &&
          match node.kind with
          | Int _ -> false
          | _ -> true
        then (
          Hash_set.add already_scheduled node;
          let deps = Graph.get_dependencies g node in
          List.filter_opt deps |> List.iter ~f:schedule;
          let scheduled_n, _ =
              List.fold
                (List.tl deps |> Option.value ~default:[])
                ~init:(Graph.get_start g, idepth g (Graph.get_start g))
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
          if not (Poly.equal node.kind (Ideal Phi)) then
            Graph.set_dependency g node (Some scheduled_n) 0)
    in
    rev_post_order_bfs_ctrl g (Graph.get_start g)
    |> List.iter ~f:(fun n ->
           let nodes =
               match n.kind with
               | Ideal Region
               | Ideal Loop ->
                   (* TODO consider filtering for phis? don't think it's needed for now though, only phis or ctrl nodes are dependants of regions *)
                   Graph.get_dependants g n
               | _ -> Graph.get_dependencies g n |> List.filter_opt
           in
           List.iter nodes ~f:(fun n ->
               match n.kind with
               | _ when Machine_node.is_control_node n -> ()
               | _ -> schedule n))

let schedule_late (g : Machine_node.t Graph.t) =
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
    let rec dom (n : Machine_node.t) =
        match n.kind with
        | Ideal Start -> assert false
        | Ideal Loop -> Loop_node.get_entry_edge g n
        | Ideal Region ->
            Graph.get_dependencies g n |> List.filter_opt |> List.reduce_exn ~f:common_dom
        | _ -> Graph.get_dependency g n 0 |> Option.value_exn
    and common_dom (lhs : Machine_node.t) (rhs : Machine_node.t) =
        if Machine_node.equal lhs rhs then
          lhs
        else
          let comp = idepth g lhs - idepth g rhs in
          if comp >= 0 then
            common_dom (dom lhs) rhs
          else
            common_dom lhs (dom rhs)
    in
    let find_best early late =
        let rec get_path cur =
            if Machine_node.equal cur early then
              [ early ]
            else
              cur :: get_path (dom cur)
        in
        let ( < ) a b =
            loop_depth g a > loop_depth g b
            || idepth g a < idepth g b
            ||
            match a.kind with
            | Jmp _ -> true
            | _ -> false
        in
        List.max_elt (get_path late) ~compare:(fun a b -> if a < b then -1 else 1)
        |> Option.value_exn
    in
    let rec schedule (node : Machine_node.t) =
        if not (Hashtbl.mem m node) then (
          (match node.kind with
          | Ideal Phi ->
              Hashtbl.set m ~key:node ~data:(Graph.get_dependency g node 0 |> Option.value_exn)
          | _ when Machine_node.is_control_node node -> Hashtbl.set m ~key:node ~data:node
          | _ -> ());
          List.iter (Graph.get_dependants g node) ~f:(fun n ->
              if is_forward_edge node n then schedule n);
          match node.kind with
          | Ideal Phi
          | _
            when Machine_node.is_control_node node ->
              ()
          | _ ->
              let lca =
                  List.map (Graph.get_dependants g node) ~f:(get_block node)
                  |> List.reduce_exn ~f:common_dom
              in
              let early = Graph.get_dependency g node 0 |> Option.value_exn in
              let best = find_best early lca in
              Hashtbl.set m ~key:node ~data:best)
    in
    schedule (Graph.get_start g);
    Hashtbl.iteri m ~f:(fun ~key ~data ->
        match key.kind with
        | Ideal Phi -> ()
        | _ when Machine_node.is_control_node key -> ()
        | _ -> Graph.set_dependency g key (Some data) 0)

let score (g : Machine_node.t Graph.t) (n : Machine_node.t) =
    match n.kind with
    | DProj _
    | Ideal (CProj _) ->
        1001
    | Ideal Phi -> 1000
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
    let cfg = rev_post_order_bfs_ctrl g (Graph.get_start g) in
    cfg
    |> List.filter ~f:Machine_node.is_blockhead
    |> List.map ~f:(fun bb ->
           bb :: (Graph.get_dependants g bb |> List.filter ~f:(Fun.negate Machine_node.is_blockhead))
           |> schedule_main)

let schedule (g : Machine_node.t Graph.t) =
    (* FIXME schedule early pulls out nodes from branches of an if to before the if. They then get pulled back in to the branch in schedule_flat but it still feels wrong for schedule_early to be able to pull them out *)
    schedule_early g;
    schedule_late g;
    schedule_flat g
