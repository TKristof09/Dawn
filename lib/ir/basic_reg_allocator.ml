open Core

module ComparableNode = struct
  include Machine_node
  include Comparator.Make (Machine_node)
end

module NodeSet = Set.Make_plain (ComparableNode)

module Range = struct
  type t = NodeSet.t * Registers.Mask.t * int * int [@@deriving sexp_of]

  let show (s, regs, first, last) =
      let s = Set.to_list s |> [%derive.show: Machine_node.t list] in
      Printf.sprintf "[%d - %d]: %s --> %s" first last s (Registers.Mask.show regs)

  let compare (s1, _, _, _) (s2, _, _, _) = Set.compare_direct s1 s2

  let hash (s, _, _, _) =
      let state = Hash.alloc () in
      Set.fold s ~init:state ~f:(fun s n -> Hash.fold_int s (Machine_node.hash n))
      |> Hash.get_hash_value
end

let clone_node g (n : Machine_node.t) =
    let n' = { n with id = Machine_node.next_id () } in
    Graph.add_dependencies g n' (Graph.get_dependencies g n);
    n'

let copy_node g (n : Machine_node.t) =
    let n' : Machine_node.t =
        {
          id = Machine_node.next_id ();
          kind = Mov;
          ir_node = n.ir_node;
          in_regs = Machine_node.get_in_reg_mask Mov;
          out_reg = Machine_node.get_out_reg_mask Mov;
        }
    in
    let cfg = Graph.get_dependency g n 0 in
    Graph.add_dependencies g n' [ cfg; Some n ];
    n'

type basic_block = {
    head : Machine_node.t;
    nodes : Machine_node.t list;
  }

let find_conflicts (g : Machine_node.t Graph.t) (program : Machine_node.t list list)
    (ranges : (Machine_node.t, NodeSet.t) Hashtbl.t) =
    let bb_outs = Hashtbl.create (module Machine_node) in
    let bbs =
        match
          Hashtbl.create_mapped
            (module Machine_node)
            ~get_key:List.hd_exn
            ~get_data:(fun l ->
              { head = List.hd_exn l; nodes = List.tl l |> Option.value ~default:[] })
            program
        with
        | `Ok m -> m
        | `Duplicate_keys _ -> assert false
    in
    let conflicts = Hashtbl.Poly.create () in
    let update_bb_outs bb (actives : (NodeSet.t, Machine_node.t) Hashtbl.t) =
        let rec get_blockhead n =
            if Machine_node.is_blockhead n then
              n
            else
              Graph.get_dependency g n 0 |> Option.value_exn |> get_blockhead
        in
        let precs =
            match bb.head.kind with
            | Ideal Region -> Graph.get_dependencies g bb.head
            | Ideal Loop -> Graph.get_dependencies g bb.head
            | Ideal Start -> []
            | _ -> [ Graph.get_dependency g bb.head 0 ]
        in
        List.map precs ~f:(Option.map ~f:get_blockhead)
        |> List.filteri ~f:(fun i prec ->
               match prec with
               | None -> false
               | Some prec ->
                   let prec_live_outs =
                       Hashtbl.find_or_add bb_outs prec ~default:(fun _ -> Hashtbl.Poly.create ())
                   in
                   Hashtbl.fold actives ~init:false ~f:(fun ~key:lrg ~data:n acc ->
                       let def =
                           match n.kind with
                           | Ideal Phi ->
                               if
                                 Machine_node.equal
                                   (Graph.get_dependency g n 0 |> Option.value_exn)
                                   bb.head
                               then (
                                 assert (i <> 0);
                                 Graph.get_dependency g n i |> Option.value_exn)
                               else
                                 n
                           | _ -> n
                       in
                       match Hashtbl.find prec_live_outs lrg with
                       | None ->
                           Hashtbl.set prec_live_outs ~key:lrg ~data:def;
                           true
                       | Some n' ->
                           if Machine_node.equal def n' then
                             ()
                           else
                             Hashtbl.update conflicts lrg ~f:(function
                               | None -> NodeSet.of_list [ def; n' ]
                               | Some s ->
                                   let s = Set.add s def in
                                   Set.add s n');
                           acc))
        |> List.filter_opt
        |> List.map ~f:(Hashtbl.find_exn bbs)
    in
    let get_actives bb =
        match Hashtbl.find bb_outs bb.head with
        | None -> Hashtbl.Poly.create ()
        | Some outs -> Hashtbl.copy outs
    in
    let check_self_conflict actives range n =
        match Hashtbl.find actives range with
        | Some other ->
            if Machine_node.equal n other then
              None
            else
              Some other
        | None -> None
    in
    let do_block (bb : basic_block) =
        (* lrg -> bb where the lrg is defined *)
        let actives = get_actives bb in
        List.rev bb.nodes
        |> List.filter ~f:(fun n ->
               match n.kind with
               | Ideal _ -> false
               | _ -> true)
        |> List.iter ~f:(fun node ->
               match Hashtbl.find ranges node with
               | None -> () (* not every node is in a range, e.g jmp nodes (if) *)
               | Some range ->
                   (* check def side conflict. eg. if there is already a def y for the lrg and node != y then conflict *)
                   (match check_self_conflict actives range node with
                   | Some other ->
                       Hashtbl.update conflicts range ~f:(function
                         | None -> NodeSet.of_list [ node; other ]
                         | Some s ->
                             let s = Set.add s node in
                             Set.add s other)
                   | None -> ());
                   (* remove the def *)
                   Hashtbl.remove actives range;
                   (* add the nodes it uses *)
                   Graph.get_dependencies g node
                   |> List.tl
                   |> Option.value ~default:[]
                   |> List.iter ~f:(function
                        | None -> ()
                        | Some dep -> (
                            match Hashtbl.find ranges dep with
                            | None -> ()
                            | Some r ->
                                (match check_self_conflict actives r dep with
                                | Some other ->
                                    Hashtbl.update conflicts r ~f:(function
                                      | None -> NodeSet.of_list [ dep; other ]
                                      | Some s ->
                                          let s = Set.add s dep in
                                          Set.add s other)
                                | None -> ());
                                Hashtbl.set actives ~key:r ~data:dep)));
        update_bb_outs bb actives
    in
    let queue = Queue.create () in
    List.iter program ~f:(fun l ->
        (* we want the blocks in reverse order *)
        let bb = { head = List.hd_exn l; nodes = List.tl l |> Option.value ~default:[] } in
        Queue.enqueue_front queue bb);
    while not (Queue.is_empty queue) do
      let bb = Queue.dequeue_exn queue in
      let changed = do_block bb in
      Queue.enqueue_all queue changed
    done;
    conflicts

let resolve_conflicts (g : Machine_node.t Graph.t) (program : Machine_node.t list)
    (conflicts : (NodeSet.t, NodeSet.t) Hashtbl.t) =
    (* TODO this is probably not good, idk how we want to represent conflicts (and if this resolution is even correct) *)
    let conflicts = Hashtbl.data conflicts |> NodeSet.union_list |> Set.to_list in
    let rec insert_before_normal program ~node_to_insert ~(before_this : Machine_node.t) =
        assert (not (Poly.equal before_this.kind (Ideal Phi)));
        match program with
        | [] -> []
        | h :: t when Machine_node.equal h before_this ->
            let n' =
                if Machine_node.is_cheap_to_clone node_to_insert then
                  clone_node g node_to_insert
                else
                  copy_node g node_to_insert
            in
            let bb = Graph.get_dependency g before_this 0 in
            Graph.set_dependency g n' bb 0;
            let dep_idx, _ =
                List.findi_exn (Graph.get_dependencies g before_this) ~f:(fun _ dep ->
                    match dep with
                    | None -> false
                    | Some dep -> Machine_node.equal dep node_to_insert)
            in
            Graph.set_dependency g before_this (Some n') dep_idx;
            n' :: h :: t
        | h :: t -> h :: insert_before t ~node_to_insert ~before_this
    and insert_before_phi program ~node_to_insert ~(before_this : Machine_node.t) =
        assert (Poly.equal before_this.kind (Ideal Phi));
        let idx =
            Graph.get_dependencies g before_this
            |> List.find_mapi_exn ~f:(fun i n ->
                   match n with
                   | None -> None
                   | Some n -> if Machine_node.equal n node_to_insert then Some i else None)
        in
        let region = Graph.get_dependency g before_this 0 |> Option.value_exn in
        let bb = Graph.get_dependency g region idx |> Option.value_exn in
        let end_of_bb =
            List.fold_until (List.tl_exn program) ~init:None
              ~f:(fun acc n ->
                let cfg = if Machine_node.equal n bb then Some n else Graph.get_dependency g n 0 in
                match cfg with
                | None -> Continue acc
                | Some cfg -> (
                    match acc with
                    | None -> if Machine_node.equal bb cfg then Continue (Some n) else Continue None
                    | Some n' ->
                        if Machine_node.equal bb cfg then Continue (Some n) else Stop (Some n')))
              ~finish:(fun res ->
                match res with
                | Some n -> Some n
                | None -> failwithf "Couldn't find end of bb %s" (Machine_node.show bb) ())
            |> Option.value_exn
        in
        let rec aux l =
            match l with
            | [] -> []
            | h :: t when Machine_node.equal h end_of_bb ->
                let n' =
                    if Machine_node.is_cheap_to_clone node_to_insert then
                      clone_node g node_to_insert
                    else
                      copy_node g node_to_insert
                in
                Graph.set_dependency g n' (Some bb) 0;
                let dep_idx, _ =
                    List.findi_exn (Graph.get_dependencies g before_this) ~f:(fun _ dep ->
                        match dep with
                        | None -> false
                        | Some dep -> Machine_node.equal dep node_to_insert)
                in
                Graph.set_dependency g before_this (Some n') dep_idx;
                h :: n' :: t
            | h :: t -> h :: aux t
        in
        aux program
    and insert_before program ~node_to_insert ~before_this =
        match before_this.kind with
        | Ideal Phi -> insert_before_phi program ~node_to_insert ~before_this
        | _ -> insert_before_normal program ~node_to_insert ~before_this
    in
    let rec duplicate_after program node =
        let rec skip_phis (l : Machine_node.t list) n =
            match l with
            | [] -> [ n ]
            | h :: t -> (
                match h.kind with
                | Ideal Phi -> h :: skip_phis t n
                | _ -> n :: l)
        in
        match program with
        | [] -> []
        | h :: _ when Machine_node.equal h node ->
            let dependants = Graph.get_dependants g node in
            let n' =
                if Machine_node.is_cheap_to_clone node then
                  clone_node g node
                else
                  copy_node g node
            in
            dependants
            |> List.iter ~f:(fun use ->
                   let dep_idx, _ =
                       List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                           match dep with
                           | None -> false
                           | Some dep -> Machine_node.equal dep node)
                   in
                   Graph.set_dependency g use (Some n') dep_idx);
            skip_phis program n'
        | h :: t -> h :: duplicate_after t node
    in

    (* TODO: same function as in scheduler.ml, deduplicate *)
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
    in
    List.fold conflicts ~init:program ~f:(fun program n ->
        let program =
            List.fold (Graph.get_dependants g n) ~init:program ~f:(fun program use ->
                match use.kind with
                | Ideal Phi ->
                    let cfg = Graph.get_dependency g use 0 |> Option.value_exn in
                    let def_cfg = Graph.get_dependency g n 0 |> Option.value_exn in
                    let is_backedge = Machine_node.equal (Loop_node.get_back_edge g use) n in
                    if
                      is_backedge
                      && Poly.equal cfg.kind (Ideal Loop)
                      && idepth g def_cfg > idepth g cfg
                    then
                      program
                    else
                      insert_before program ~node_to_insert:n ~before_this:use
                | _
                  when Machine_node.is_two_address use
                       && Machine_node.equal (Graph.get_dependency g use 1 |> Option.value_exn) n ->
                    insert_before program ~node_to_insert:n ~before_this:use
                | _ -> program)
        in
        match n.kind with
        | Ideal Phi ->
            let backedge = Loop_node.get_entry_edge g n in
            let program = duplicate_after program n in
            insert_before program ~node_to_insert:backedge ~before_this:n
        | _ when Machine_node.is_two_address n ->
            let dep = Graph.get_dependency g n 1 |> Option.value_exn in
            insert_before program ~node_to_insert:dep ~before_this:n
        | _ -> program)

let get_live_ranges (g : Machine_node.t Graph.t) (program : Machine_node.t list) =
    let ranges = Hashtbl.create (module Machine_node) in
    List.iter program ~f:(fun n ->
        match n.kind with
        | Ideal Phi ->
            (* merge all depedencies into same live range except for the control depedency, we don't care about that *)
            let depedencies = Graph.get_dependencies g n |> List.tl_exn |> List.filter_opt in
            let merged =
                NodeSet.union_list
                  (List.filter_map depedencies ~f:(fun dep ->
                       match dep.kind with
                       | Ideal t when not (Poly.equal t Phi) -> None
                       | _ ->
                           Some
                             (Hashtbl.find_or_add ranges dep ~default:(fun () ->
                                  NodeSet.singleton dep))))
            in
            let this_lrg = Hashtbl.find ranges n |> Option.value ~default:(NodeSet.singleton n) in
            let merged = Set.union merged this_lrg in
            Set.iter merged ~f:(fun n -> Hashtbl.set ranges ~key:n ~data:merged)
        | _ when Machine_node.is_two_address n ->
            let input1 = Graph.get_dependency g n 1 |> Option.value_exn in
            let input_lrg =
                Hashtbl.find_or_add ranges input1 ~default:(fun () -> NodeSet.singleton input1)
            in
            let this_lrg = Hashtbl.find ranges n |> Option.value ~default:(NodeSet.singleton n) in
            let merged = Set.union input_lrg this_lrg in
            Set.iter merged ~f:(fun n -> Hashtbl.set ranges ~key:n ~data:merged)
        | _ when Option.is_none (n.out_reg g n 0) -> ()
        | _ -> Hashtbl.add ranges ~key:n ~data:(NodeSet.singleton n) |> ignore);
    ranges

(* TODO: this is basically the same calculations as in find_conflicts, so they should be merged together *)
let compute_liveness (g : Machine_node.t Graph.t) (program : Machine_node.t list)
    (ranges : (Machine_node.t, NodeSet.t) Hashtbl.t) =
    let program = List.group program ~break:(fun _ n -> Machine_node.is_blockhead n) in
    let bb_outs = Hashtbl.create (module Machine_node) in
    let bbs =
        match
          Hashtbl.create_mapped
            (module Machine_node)
            ~get_key:List.hd_exn
            ~get_data:(fun l ->
              { head = List.hd_exn l; nodes = List.tl l |> Option.value ~default:[] })
            program
        with
        | `Ok m -> m
        | `Duplicate_keys _ -> assert false
    in
    let range_uses = Hashtbl.Poly.create () in
    let update_bb_outs bb (actives : (NodeSet.t, Machine_node.t) Hashtbl.t) =
        let rec get_blockhead n =
            if Machine_node.is_blockhead n then
              n
            else
              Graph.get_dependency g n 0 |> Option.value_exn |> get_blockhead
        in
        let precs =
            match bb.head.kind with
            | Ideal Region -> Graph.get_dependencies g bb.head
            | Ideal Loop -> Graph.get_dependencies g bb.head
            | Ideal Start -> []
            | _ -> [ Graph.get_dependency g bb.head 0 ]
        in
        List.map precs ~f:(Option.map ~f:get_blockhead)
        |> List.filteri ~f:(fun i prec ->
               match prec with
               | None -> false
               | Some prec ->
                   let prec_live_outs =
                       Hashtbl.find_or_add bb_outs prec ~default:(fun _ -> Hashtbl.Poly.create ())
                   in
                   Hashtbl.fold actives ~init:false ~f:(fun ~key:lrg ~data:n acc ->
                       let def =
                           match n.kind with
                           | Ideal Phi ->
                               if
                                 Machine_node.equal
                                   (Graph.get_dependency g n 0 |> Option.value_exn)
                                   bb.head
                               then (
                                 assert (i <> 0);
                                 Graph.get_dependency g n i |> Option.value_exn)
                               else
                                 n
                           | _ -> n
                       in
                       match Hashtbl.find prec_live_outs lrg with
                       | None ->
                           Hashtbl.set prec_live_outs ~key:lrg ~data:def;
                           let prec_bb = Hashtbl.find_exn bbs prec in
                           let last_node = List.last prec_bb.nodes |> Option.value ~default:prec in
                           Hashtbl.update range_uses lrg ~f:(function
                             | None -> [ last_node ]
                             | Some l -> last_node :: l);
                           true
                       | Some _ -> acc))
        |> List.filter_opt
        |> List.map ~f:(Hashtbl.find_exn bbs)
    in
    let get_actives bb =
        match Hashtbl.find bb_outs bb.head with
        | None -> Hashtbl.Poly.create ()
        | Some outs -> Hashtbl.copy outs
    in
    let do_block (bb : basic_block) =
        (* lrg -> bb where the lrg is defined *)
        let actives = get_actives bb in
        List.rev bb.nodes
        |> List.filter ~f:(fun n ->
               match n.kind with
               | Ideal _ -> false
               | _ -> true)
        |> List.iter ~f:(fun node ->
               (match Hashtbl.find ranges node with
               | None -> () (* not every node is in a range, e.g jmp nodes (if) *)
               | Some range ->
                   (* remove the def *)
                   Hashtbl.remove actives range);
               (* add the nodes it uses *)
               Graph.get_dependencies g node
               |> List.tl
               |> Option.value ~default:[]
               |> List.filter_opt
               |> List.iter ~f:(fun dep ->
                      match Hashtbl.find ranges dep with
                      | None -> ()
                      | Some r ->
                          Hashtbl.set actives ~key:r ~data:dep;
                          Hashtbl.update range_uses r ~f:(function
                            | None -> [ node ]
                            | Some l -> node :: l)));
        let live_ins = actives in
        update_bb_outs bb live_ins
    in
    let queue = Queue.create () in
    List.iter program ~f:(fun l ->
        (* we want the blocks in reverse order *)
        let bb = { head = List.hd_exn l; nodes = List.tl l |> Option.value ~default:[] } in
        Queue.enqueue_front queue bb);
    while not (Queue.is_empty queue) do
      let bb = Queue.dequeue_exn queue in
      let changed = do_block bb in
      Queue.enqueue_all queue changed
    done;
    range_uses

let process_range (g : Machine_node.t Graph.t) (program : Machine_node.t list)
    (range_uses : (NodeSet.t, Machine_node.t list) Hashtbl.t) (range : NodeSet.t) =
    let first_def =
        Set.to_list range
        |> List.map ~f:(fun n ->
               List.findi_exn program ~f:(fun _ n' -> Machine_node.equal n n') |> fst)
        |> List.min_elt ~compare:Int.compare
        |> Option.value_exn
    in
    let last_usage =
        Hashtbl.find_exn range_uses range
        |> List.map ~f:(fun n ->
               List.findi_exn program ~f:(fun _ n' -> Machine_node.equal n n') |> fst)
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
    in
    (* TODO figure out what reg mask is *)
    let reg_mask =
        Set.fold range ~init:Registers.Mask.all ~f:(fun mask node ->
            let uses_in_regs =
                Graph.get_dependants g node
                |> List.filter ~f:(fun use ->
                       match use.kind with
                       | Ideal _ -> false
                       | _ -> true)
                |> List.map ~f:(fun use ->
                       let dep_idx, _ =
                           List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                               match dep with
                               | None -> false
                               | Some dep -> Machine_node.equal dep node)
                       in
                       use.in_regs g use (dep_idx - 1) |> Option.value_exn)
            in
            let mask = List.fold uses_in_regs ~init:mask ~f:Registers.Mask.common in
            if Poly.equal node.kind (Ideal Phi) then
              mask
            else
              match node.out_reg g node 0 with
              | None -> Registers.Mask.empty
              | Some reg -> Registers.Mask.common mask reg)
    in
    (range, reg_mask, first_def, last_usage)

let split_range_by_cloning (g : Machine_node.t Graph.t) (program : Machine_node.t list)
    (ranges : Range.t list) (pref : Machine_node.t list) (suff : Machine_node.t list)
    (node : Machine_node.t) =
    let uses = Graph.get_dependants g node |> Hash_set.of_list (module Machine_node) in
    (* NOTE: could use dlists for fast list concats *)
    let rec insert_before_use l idx =
        match l with
        | [] -> failwith "No uses found"
        | h :: t ->
            if Hash_set.mem uses h then (
              let node' = clone_node g node in
              let later_uses =
                  Hash_set.fold uses ~init:[] ~f:(fun acc use ->
                      let use_idx =
                          List.findi_exn program ~f:(fun _ n -> Machine_node.equal n use) |> fst
                      in
                      if use_idx >= idx then use :: acc else acc)
              in
              let bb = Graph.get_dependency g h 0 in
              Graph.set_dependency g node' bb 0;
              List.iter later_uses ~f:(fun use ->
                  let dep_idx, _ =
                      List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                          match dep with
                          | None -> false
                          | Some dep -> Machine_node.equal dep node)
                  in
                  Graph.set_dependency g use (Some node') dep_idx);
              (idx, node', node' :: h :: t))
            else
              let res_idx, new_node, tail = insert_before_use t (idx + 1) in
              (res_idx, new_node, h :: tail)
    in
    let rec remove_in_prefix_and_merge l new_suff idx =
        match l with
        | [] -> (idx, new_suff)
        | h :: t ->
            if Machine_node.equal h node then
              (idx, t @ new_suff)
            else
              let delete_idx, tail = remove_in_prefix_and_merge t new_suff (idx + 1) in
              (delete_idx, h :: tail)
    in
    let insert_idx, new_node, new_suff =
        insert_before_use (List.tl_exn suff) (List.length pref + 1)
    in
    let new_suff = List.hd_exn suff :: new_suff in
    let new_program =
        if List.is_empty @@ Graph.get_dependants g node then
          (* if the node has no uses in prefix (aka no uses at all since uses in
         suffix now point to the cloned node) we can remove it as it got cloned into
         suffix already *)
          let delete_idx, new_program = remove_in_prefix_and_merge pref new_suff 0 in
          new_program
        else (* if the node has uses in the prefix we can't delete it *)
          pref @ new_suff
    in

    let live_ranges = get_live_ranges g new_program in
    let range_uses = compute_liveness g new_program live_ranges in
    let ranges =
        Hashtbl.data live_ranges
        |> List.stable_dedup ~compare:Set.compare_direct
        |> List.map ~f:(process_range g new_program range_uses)
        |> List.sort ~compare:(fun (_, _, first, last) (_, _, first', last') ->
               let c1 = Int.compare first first' in
               if c1 <> 0 then c1 else Int.compare last last')
    in
    let first_range = List.find ranges ~f:(fun (s, _, _, _) -> Set.mem s node) in
    let second_range = List.find_exn ranges ~f:(fun (s, _, _, _) -> Set.mem s new_node) in
    (ranges, new_program, first_range, second_range)

let find_splittable_by_cloning (program : Machine_node.t list) (pref : Machine_node.t list)
    (ranges : Range.t list) (mask : Registers.Mask.t) =
    let find_last instr_list range =
        let rec aux l last =
            match l with
            | [] -> last
            | h :: t when Set.mem range h -> aux t h
            | _ :: t -> aux t last
        in
        aux instr_list (List.hd_exn instr_list)
    in
    let find_idx n = List.findi_exn program ~f:(fun _ n' -> Machine_node.equal n n') |> fst in
    let compare (n, r) (n', r') =
        let s, _, _, _ = r in
        let s', _, _, _ = r' in
        let n_idx = find_idx n in
        let next_idx =
            Set.fold s ~init:Int.max_value ~f:(fun best_idx n ->
                let idx = find_idx n in
                if idx > n_idx then min best_idx idx else best_idx)
        in
        let n_idx' = find_idx n' in
        let next_idx' =
            Set.fold s' ~init:Int.max_value ~f:(fun best_idx n ->
                let idx = find_idx n in
                if idx > n_idx' then min best_idx idx else best_idx)
        in
        Int.compare next_idx next_idx'
    in
    let possible_splits =
        List.filter_map ranges ~f:(fun ((r, reg_mask, _, _) as range) ->
            if not (Registers.Mask.is_empty (Registers.Mask.common mask reg_mask)) then
              let last_in_pref = find_last pref r in
              if Machine_node.is_cheap_to_clone last_in_pref then
                Some (last_in_pref, range)
              else
                None
            else
              None)
    in
    List.max_elt possible_splits ~compare

let find_swappable range free_regs active_ranges ranges register_assoc =
    let does_overlap (start, stop) (start', stop') =
        let earlier_stop = min stop stop'
        and later_start = max start start' in
        not (earlier_stop < later_start)
    in
    let _, wanted_reg_mask, cur_start, cur_stop = range in
    let potentials =
        Set.to_list active_ranges
        |> List.filter_map ~f:(fun ((_, candidate_reg_mask, start, stop) as r) ->
               let shared_with_current = Registers.Mask.common wanted_reg_mask candidate_reg_mask in
               let free_for_candidate = Registers.Mask.common free_regs candidate_reg_mask in
               if
                 Registers.Mask.is_empty shared_with_current
                 || Registers.Mask.is_empty free_for_candidate
               then
                 None
               else
                 let unusables =
                     List.filter ranges ~f:(fun (_, _, first_def, last_use) ->
                         does_overlap (start, stop) (first_def, last_use)
                         || does_overlap (cur_start, cur_stop) (first_def, last_use))
                     |> List.fold ~init:Registers.Mask.empty ~f:(fun acc r ->
                            Registers.Mask.add acc (Hashtbl.find_exn register_assoc r))
                 in
                 let available_for_candidate = Registers.Mask.diff free_for_candidate unusables in
                 let freed_up_regs = Registers.Mask.diff shared_with_current unusables in
                 if
                   Registers.Mask.is_empty available_for_candidate
                   || Registers.Mask.is_empty freed_up_regs
                 then
                   None
                 else
                   Some (r, available_for_candidate))
    in
    List.hd potentials

let assign_registers (g : Machine_node.t Graph.t) (program : Machine_node.t list)
    (ranges : Range.t list) =
    let register_assoc = Hashtbl.create (module Range) in
    let get_sorted_endpoints ranges =
        List.fold ranges ~init:[] ~f:(fun acc ((_, _, first_def, last_use) as range) ->
            let start = min first_def last_use
            and stop = max first_def last_use in
            (start, `Start range) :: (stop, `End range) :: acc)
        |> List.sort ~compare:(fun (i, r) (i', r') ->
               (* Sort by index, but put End before Start. This allows the
                  allocator to reuse registers for ranges that touch at a
                  single point, e.g., [1,2] and [2,3] can share the same
                  register (because def "happens after" use). When we split
                  self conflicting ranges we create a lot of Movs, sorting this
                  way enables same-to-same-reg copies that get eliminated later
                  on *)
               if i = i' then
                 match (r, r') with
                 | `Start _, `End _ -> 1
                 | `End _, `Start _ -> -1
                 | _ -> 0
               else
                 Int.compare i i')
    in
    let endpoints = get_sorted_endpoints ranges in
    let rec aux l active_ranges program (free_regs : Registers.Mask.t) =
        match l with
        | [] -> program
        | (idx, `Start range) :: t ->
            let _, reg_mask, _, _ = range in
            let new_endpoints, new_program, reg_used =
                match Registers.Mask.common free_regs reg_mask |> Registers.Mask.choose with
                | Some r ->
                    Hashtbl.set register_assoc ~key:range ~data:r;
                    (t, program, Some r)
                | None -> (
                    let possible_swap =
                        if Registers.Mask.is_empty free_regs then
                          None
                        else
                          find_swappable range free_regs active_ranges ranges register_assoc
                    in
                    match possible_swap with
                    | Some (candidate_range, available_for_candidate) ->
                        let candidate_old_reg = Hashtbl.find_exn register_assoc candidate_range in
                        let candidate_new_reg =
                            Registers.Mask.choose available_for_candidate |> Option.value_exn
                        in
                        Hashtbl.set register_assoc ~key:range ~data:candidate_old_reg;
                        Hashtbl.set register_assoc ~key:candidate_range ~data:candidate_new_reg;
                        (t, program, Some candidate_new_reg)
                    | None -> (
                        let pref, suff = List.split_n program (idx - 1 + 1) in
                        match find_splittable_by_cloning program pref ranges reg_mask with
                        | None -> failwithf "TODO spill %s" (Range.show range) ()
                        | Some (split_after_node, candidate_range) ->
                            let new_ranges, new_program, split_range1, split_range2 =
                                split_range_by_cloning g program ranges pref suff split_after_node
                            in
                            let old_reg =
                                Hashtbl.find_and_remove register_assoc candidate_range
                                |> Option.value_exn
                            in
                            (match split_range1 with
                            | None -> ()
                            | Some split_range1 ->
                                Hashtbl.set register_assoc ~key:split_range1 ~data:old_reg);
                            Hashtbl.set register_assoc ~key:range ~data:old_reg;
                            let new_endpoints = get_sorted_endpoints new_ranges in
                            let new_endpoints =
                                List.drop_while new_endpoints ~f:(fun (i, _) -> i <= idx)
                            in
                            (new_endpoints, new_program, None)))
            in
            let free_regs =
                match reg_used with
                | None -> free_regs
                | Some r -> Registers.Mask.remove free_regs r
            in
            aux new_endpoints (Set.add active_ranges range) new_program free_regs
        | (_, `End range) :: t ->
            let free_regs = Hashtbl.find_exn register_assoc range |> Registers.Mask.add free_regs in
            aux t (Set.remove active_ranges range) program free_regs
    in
    let new_program = aux endpoints Set.Poly.empty program Registers.Mask.all in
    (register_assoc, new_program)

let cleanup_movs g (program : Machine_node.t list) register_assoc =
    List.filter program ~f:(fun n ->
        match n.kind with
        | Mov ->
            let dep = Graph.get_dependency g n 1 |> Option.value_exn in
            let dst_reg = Hashtbl.find_exn register_assoc n in
            let src_reg = Hashtbl.find_exn register_assoc dep in
            if Poly.equal src_reg dst_reg then (
              Graph.replace_node_with g n dep;
              false)
            else
              true
        | _ -> true)

let expand_register_assoc register_assoc =
    let res = Hashtbl.create (module Machine_node) in
    Hashtbl.iteri register_assoc ~f:(fun ~key:range ~data:reg ->
        let s, _, _, _ = range in
        Set.iter s ~f:(fun n -> Hashtbl.set res ~key:n ~data:reg));
    res

let allocate (g : Machine_node.t Graph.t) (program : Machine_node.t list list) =
    let flat_program = List.concat program in
    Ir_printer.to_string_machine_linear g flat_program |> Printf.printf "%s\n";
    let live_ranges = get_live_ranges g flat_program in
    let conflicts = find_conflicts g program live_ranges in
    conflicts
    |> Hashtbl.data
    |> NodeSet.union_list
    |> Set.to_list
    |> [%derive.show: Machine_node.t list]
    |> Printf.printf "\nCONFLICTS: {\n%s\n}\n";
    let program = resolve_conflicts g flat_program conflicts in

    Graph.cleanup g;
    Graph.get_dependants g (Graph.get_start g)
    |> List.iter ~f:(fun n ->
           if Graph.get_dependants g n |> List.is_empty then Graph.remove_node g n);
    let program =
        List.filter program ~f:(fun n ->
            Graph.find g ~f:(fun n' -> Machine_node.equal n n') |> Option.is_some)
    in
    let live_ranges = get_live_ranges g program in
    let range_uses = compute_liveness g program live_ranges in
    (* Hashtbl.iteri range_uses ~f:(fun ~key ~data -> *)
    (*     Printf.printf "%s ----------- %s\n\n\n" *)
    (*       (Set.to_list key |> [%derive.show: Machine_node.t list]) *)
    (*       ([%derive.show: Machine_node.t list] data)); *)
    let ranges =
        Hashtbl.data live_ranges
        |> List.stable_dedup ~compare:Set.compare_direct
        |> List.map ~f:(process_range g program range_uses)
        |> List.sort ~compare:(fun (_, _, first, last) (_, _, first', last') ->
               let c1 = Int.compare first first' in
               if c1 <> 0 then c1 else Int.compare last last')
    in
    Printf.printf "RANGES: \n%s\n" (List.map ranges ~f:Range.show |> String.concat ~sep:";\n");
    let register_assoc, program = assign_registers g program ranges in
    (* Printf.printf "=================== ALLOCATION RESULTS =============================\n"; *)
    (* Hashtbl.iteri register_assoc ~f:(fun ~key ~data -> *)
    (*     let _, _, first, last = key in *)
    (*     Printf.printf "[%d - %d] -> %s\n" first last (Registers.show_reg data)); *)
    let register_assoc = expand_register_assoc register_assoc in
    let program = cleanup_movs g program register_assoc in
    (* List.iter program ~f:(fun n -> *)
    (*     match n.kind with *)
    (*     | Ideal _ -> Printf.printf "(%s)\n" (Machine_node.show_machine_node_kind n.kind) *)
    (*     | _ -> ( *)
    (*         match Hashtbl.find register_assoc n with *)
    (*         | None -> () *)
    (*         | Some reg -> *)
    (*             Printf.printf "#%s = %s " (Registers.show_reg reg) *)
    (*               (Machine_node.show_machine_node_kind n.kind); *)
    (*             Graph.get_dependencies g n *)
    (*             |> List.tl *)
    (*             |> Option.value ~default:[] *)
    (*             |> List.filter_opt *)
    (*             |> List.iter ~f:(fun dep -> *)
    (*                    Printf.printf "#%s," *)
    (*                      (Registers.show_reg (Hashtbl.find_exn register_assoc dep))); *)
    (*             Printf.printf "\n")); *)
    Ir_printer.to_string_machine_linear g program |> Printf.printf "%s\n";
    Ir_printer.to_string_machine_linear_regs g program register_assoc |> Printf.printf "%s\n";
    (program, register_assoc)
