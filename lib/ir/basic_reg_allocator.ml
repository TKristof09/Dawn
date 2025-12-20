open Core

module ComparableNode = struct
  include Machine_node
  include Comparator.Make (Machine_node)
end

let get_live_ranges (g : Machine_node.t Graph.t) (program : Machine_node.t list) =
    let ranges = Hashtbl.create (module Machine_node) in
    List.iter program ~f:(fun n ->
        match n.kind with
        | Ideal Phi ->
            (* merge all depedencies into same live range except for the control depedency, we don't care about that *)
            let depedencies = List.drop (Graph.get_dependencies g n) 1 |> List.filter_opt in
            let merged =
                Set.union_list
                  (module ComparableNode)
                  (List.filter_map depedencies ~f:(fun dep ->
                       match dep.kind with
                       | Ideal t when not (Poly.equal t Phi) -> None
                       | _ ->
                           Some
                             (Hashtbl.find_or_add ranges dep ~default:(fun () ->
                                  Set.singleton (module ComparableNode) dep))))
            in
            let merged = Set.add merged n in
            Set.iter merged ~f:(fun n -> Hashtbl.set ranges ~key:n ~data:merged)
        | _ when Option.is_none (n.out_reg g n 0) -> ()
        | _ -> Hashtbl.add ranges ~key:n ~data:(Set.singleton (module ComparableNode) n) |> ignore);
    ranges

let allocate (g : Machine_node.t Graph.t) (program : Machine_node.t list) =
    let live_ranges = get_live_ranges g program in
    let ranges =
        Hashtbl.data live_ranges
        |> List.stable_dedup ~compare:Set.compare_direct
        |> List.map ~f:(fun range ->
               let indices =
                   Set.to_list range
                   |> List.map ~f:(fun n ->
                          List.findi_exn program ~f:(fun _ n' -> Machine_node.equal n n') |> fst)
               in
               let first_def = List.min_elt indices ~compare:Int.compare |> Option.value_exn in
               let last_usage =
                   Set.fold range ~init:0 ~f:(fun acc n ->
                       let max_usage =
                           Graph.get_dependants g n
                           |> List.fold ~init:0 ~f:(fun max_idx dep ->
                                  let idx =
                                      List.findi_exn program ~f:(fun _ n' ->
                                          Machine_node.equal dep n')
                                      |> fst
                                  in
                                  max max_idx idx)
                       in
                       max acc max_usage)
               in
               let in_reg_mask =
                   Set.fold range ~init:Registers.Mask.all ~f:(fun mask node ->
                       Graph.get_dependencies g node
                       |> List.tl_exn
                       |> List.mapi ~f:(fun i _ -> node.in_regs g node i)
                       |> List.filter_opt
                       |> List.fold ~init:mask ~f:Registers.Mask.common)
               and out_reg_mask =
                   Set.fold range ~init:Registers.Mask.all ~f:(fun mask node ->
                       match node.out_reg g node 0 with
                       | None -> Registers.Mask.empty
                       | Some reg -> Registers.Mask.common mask reg)
               in
               (range, in_reg_mask, out_reg_mask, first_def, last_usage))
    in

    Printf.printf "{\n";
    List.iter ranges ~f:(fun (range, in_regs, out_regs, min, max) ->
        Printf.printf "%d -- %d: IN: %s OUT: %s :\n %s\n\n" min max (Registers.Mask.show in_regs)
          (Registers.Mask.show out_regs)
          ([%derive.show: Machine_node.t list] (Set.to_list range)));
    Printf.printf "}\n";
    program
