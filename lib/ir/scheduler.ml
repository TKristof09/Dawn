open Core

let rev_post_order_bfs_ctrl g node f =
    let visited = Hash_set.create (module Node) in
    Hash_set.add visited node;

    let rec dfs (node : Node.t) acc =
        match node.kind with
        | Ctrl _ ->
            Hash_set.add visited node;

            let new_acc =
                List.fold_right (Graph.get_dependants g node)
                  ~f:(fun dep current_acc ->
                    if not (Hash_set.mem visited dep) then
                      dfs dep current_acc
                    else
                      current_acc)
                  ~init:acc
            in

            node :: new_acc
        | _ -> acc
    in

    let nodes = dfs node [] in
    List.iter nodes ~f

(* consider caching this *)
let rec idepth g (node : Node.t) =
    match node.kind with
    | Ctrl Start -> 0
    | Ctrl Region ->
        Graph.get_dependencies g node
        |> List.fold ~init:0 ~f:(fun acc n ->
               match n with
               | Some n -> max acc (idepth g n)
               | _ -> assert false)
    | Ctrl Loop -> 1 + idepth g (Graph.get_dependency g node 1 |> Option.value_exn)
    | Ctrl _ -> 1 + idepth g (Graph.get_dependency g node 0 |> Option.value_exn)
    | Data _ -> assert false
    | Scope _ -> assert false

(* consider caching this *)
let rec loop_depth g (n : Node.t) =
    match n.kind with
    | Ctrl Loop ->
        let entry_depth = Graph.get_dependency g n 1 |> Option.value_exn |> loop_depth g in
        entry_depth + 1
    | Ctrl Start -> 1
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
          let deps = Graph.get_dependencies g node |> List.filter_opt in
          List.iter deps ~f:schedule;
          let scheduled_n, _ =
              List.fold deps
                ~init:(Graph.get_start g, 0)
                ~f:(fun (max_n, max_depth) n ->
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
    rev_post_order_bfs_ctrl g (Graph.get_start g) (fun n ->
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
        | Ctrl Loop ->
            Graph.get_dependency g dependant 0 |> Option.value_exn |> Node.hard_equal node
        | Data Phi -> (
            match (Graph.get_dependency g dependant 0 |> Option.value_exn).kind with
            | Ctrl Loop ->
                Graph.get_dependency g dependant 1 |> Option.value_exn |> Node.hard_equal node
            | _ -> false)
        | _ -> false
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
    let dom (n : Node.t) =
        match n.kind with
        | Ctrl Loop -> assert false
        | Ctrl Region -> assert false
        | _ -> Graph.get_dependency g n 0 |> Option.value_exn
    in
    let rec common_dom lhs rhs =
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
              []
            else
              late :: get_path (dom late)
        in
        let ( < ) a b = loop_depth g a > loop_depth g b in
        List.max_elt (get_path late) ~compare:(fun a b -> if a < b then 1 else -1)
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
          | _ ->
              let lca =
                  List.fold (Graph.get_dependants g node) ~init:node ~f:(fun lca n ->
                      get_block node n |> common_dom lca)
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
