open Core

let rev_post_order_bfs_ctrl g node =
    let visited = Hash_set.create (module Node) in
    Hash_set.add visited node;

    let rec dfs (node : Node.t) acc =
        match node.kind with
        | Ctrl _ ->
            Hash_set.add visited node;

            let new_acc =
                List.fold (Graph.get_dependants g node)
                  ~f:(fun current_acc dep ->
                    if not (Hash_set.mem visited dep) then
                      dfs dep current_acc
                    else
                      current_acc)
                  ~init:acc
            in

            node :: new_acc
        | _ -> acc
    in
    dfs node []

(* consider caching this *)
let rec idepth g (node : Node.t) =
    match node.kind with
    | Ctrl Start -> 0
    | Ctrl Region ->
        Graph.get_dependencies g node
        |> List.fold ~init:0 ~f:(fun acc n ->
               match n with
               | Some n -> max acc (idepth g n)
               | None -> acc)
    | Ctrl Loop -> 1 + idepth g (Loop_node.get_entry_edge g node)
    | Ctrl _ -> 1 + idepth g (Graph.get_dependency g node 0 |> Option.value_exn)
    | Data _ -> idepth g (Graph.get_dependency g node 0 |> Option.value_exn)
    | Scope _ -> failwith @@ Printf.sprintf "Idepth called with: %s\n" (Node.show node)

(* consider caching this *)
let rec loop_depth g (n : Node.t) =
    match n.kind with
    | Ctrl Loop ->
        let entry_depth = Loop_node.get_entry_edge g n |> loop_depth g in
        entry_depth + 1
    | Ctrl Start -> 1
    | Ctrl Region -> Graph.get_dependency g n 1 |> Option.value_exn |> loop_depth g
    | _ -> Graph.get_dependency g n 0 |> Option.value_exn |> loop_depth g

let schedule_early g =
    let already_scheduled = Hash_set.create ~size:(Graph.get_num_nodes g) (module Node) in
    let rec schedule (node : Node.t) =
        Out_channel.flush Out_channel.stdout;
        if
          (not (Hash_set.mem already_scheduled node))
          && (not (Node.is_ctrl node))
          && not (Poly.equal node.kind (Data Constant))
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
          if not (Poly.equal node.kind (Data Phi)) then
            Graph.set_dependency g node (Some scheduled_n) 0)
    in
    rev_post_order_bfs_ctrl g (Graph.get_start g)
    |> List.iter ~f:(fun n ->
           let nodes =
               match n.kind with
               | Ctrl Region
               | Ctrl Loop ->
                   (* TODO consider filtering for phis? don't think it's needed for now though, only phis or ctrl nodes are dependants of regions *)
                   Graph.get_dependants g n
               | _ -> Graph.get_dependencies g n |> List.filter_opt
           in
           List.iter nodes ~f:(fun n ->
               match n.kind with
               | Ctrl _ -> ()
               | _ -> schedule n))

let schedule_late g =
    let m = Hashtbl.create ~size:(Graph.get_num_nodes g) (module Node) in
    let is_forward_edge node (dependant : Node.t) =
        match dependant.kind with
        | Ctrl Loop -> not (Loop_node.get_back_edge g dependant |> Node.hard_equal node)
        | Data Phi -> (
            match (Graph.get_dependency g dependant 0 |> Option.value_exn).kind with
            | Ctrl Loop -> not (Loop_node.get_back_edge g dependant |> Node.hard_equal node)
            | _ -> true)
        | _ -> true
    in
    let get_block node (dependant : Node.t) =
        match dependant.kind with
        | Data Phi ->
            let idx =
                Stdlib.List.find_index
                  (function
                    | None -> false
                    | Some n -> Node.hard_equal n node)
                  (Graph.get_dependencies g dependant)
                |> Option.value_exn
            in
            let region = Graph.get_dependency g dependant 0 |> Option.value_exn in
            Graph.get_dependency g region idx |> Option.value_exn
        | _ -> Hashtbl.find_exn m dependant
    in
    let rec dom (n : Node.t) =
        match n.kind with
        | Ctrl Start -> assert false
        | Ctrl Loop -> Loop_node.get_entry_edge g n
        | Ctrl Region ->
            Graph.get_dependencies g n |> List.filter_opt |> List.reduce_exn ~f:common_dom
        | _ -> Graph.get_dependency g n 0 |> Option.value_exn
    and common_dom lhs rhs =
        if Node.hard_equal lhs rhs then
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
            if Node.hard_equal cur early then
              [ early ]
            else
              cur :: get_path (dom cur)
        in
        let ( < ) a b =
            loop_depth g a > loop_depth g b
            || idepth g a < idepth g b
            || Poly.equal a.kind (Ctrl If)
        in
        List.max_elt (get_path late) ~compare:(fun a b -> if a < b then -1 else 1)
        |> Option.value_exn
    in
    let rec schedule (node : Node.t) =
        if not (Hashtbl.mem m node) then (
          (match node.kind with
          | Data Phi ->
              Hashtbl.set m ~key:node ~data:(Graph.get_dependency g node 0 |> Option.value_exn)
          | Ctrl _ -> Hashtbl.set m ~key:node ~data:node
          | _ -> ());
          List.iter (Graph.get_dependants g node) ~f:(fun n ->
              if is_forward_edge node n then schedule n);
          match node.kind with
          | Data Phi
          | Ctrl _ ->
              ()
          | Scope _ -> ()
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
        | Data Phi
        | Ctrl _ ->
            ()
        | _ -> Graph.set_dependency g key (Some data) 0)

let schedule_flat g =
    let score (n : Node.t) =
        match n.kind with
        | Data (Proj _)
        | Ctrl (Proj _) ->
            1001
        | Data Phi -> 1000
        | Ctrl _ -> 1
        | _ -> 500
    in
    let schedule_main nodes =
        let scheduled = Dynarray.create () in
        let not_ready = Hash_set.of_list (module Node) nodes in
        let ready =
            Hash_set.filter not_ready ~f:(fun n ->
                let deps =
                    Graph.get_dependencies g n |> List.filter_opt |> Hash_set.of_list (module Node)
                in
                Hash_set.for_all deps ~f:(fun x -> not (Hash_set.mem not_ready x)))
        in
        let not_ready = Hash_set.diff not_ready ready in
        (*Printf.printf "READY: %s\n" ([%derive.show: Node.t list] (Hash_set.to_list ready));*)
        (*Printf.printf "NOT  READY: %s\n" ([%derive.show: Node.t list] (Hash_set.to_list not_ready));*)
        let is_ready node =
            List.for_all
              (Graph.get_dependencies g node |> List.tl_exn)
              ~f:(function
                | None -> true
                | Some n -> (not (Hash_set.mem ready n)) && not (Hash_set.mem not_ready n))
        in
        while not (Hash_set.is_empty ready && Hash_set.is_empty not_ready) do
          let best =
              Hash_set.max_elt ready ~compare:(fun a b -> Int.compare (score a) (score b))
              |> Option.value_exn (* ready should never be empty if not_ready isnt empty *)
          in
          Dynarray.add_last scheduled best;
          Hash_set.remove ready best;
          List.iter (Graph.get_dependants g best) ~f:(fun n ->
              if Hash_set.mem not_ready n && is_ready n then (
                (*Printf.printf "Moving %s from %s\n" (Node.show n) (Node.show best);*)
                Hash_set.add ready n;
                Hash_set.remove not_ready n))
        done;
        Dynarray.to_list scheduled
    in
    let cfg = rev_post_order_bfs_ctrl g (Graph.get_start g) in
    cfg
    |> List.filter ~f:Node.is_blockhead
    |> List.map ~f:(fun bb -> bb :: Graph.get_dependants g bb |> schedule_main)
