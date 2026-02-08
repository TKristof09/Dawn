open Core

module NodeSet = struct
  include Set.Make_plain (struct
    include Machine_node
    include Comparator.Make (Machine_node)
  end)

  let show s = Set.to_list s |> [%derive.show: Machine_node.t list] |> Printf.sprintf "%s"
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Range = struct
  type t = {
      mutable reg_mask : Registers.Mask.t;
      nodes : NodeSet.t;
    }
  [@@deriving sexp_of]

  let show t =
      Printf.sprintf "%s\n\t\t--->%s" (NodeSet.show t.nodes) (Registers.Mask.show t.reg_mask)

  let pp fmt r = Format.fprintf fmt "%s" (show r)
  let compare t t' = Set.compare_direct t.nodes t'.nodes
  let equal t t' = Set.equal t.nodes t'.nodes

  let hash t =
      let state = Hash.alloc () in
      Set.fold t.nodes ~init:state ~f:(fun s n -> Hash.fold_int s (Machine_node.hash n))
      |> Hash.get_hash_value
end

module RangeSet = struct
  include Set.Make_plain (struct
    include Range
    include Comparator.Make (Range)
  end)

  let show s = Set.to_list s |> [%derive.show: Range.t list] |> Printf.sprintf "%s"
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

let build_live_ranges (g : (Machine_node.t, 'a) Graph.t) (program : Machine_node.t list) =
    let ranges = Hashtbl.create (module Machine_node) in
    List.iter program ~f:(fun n ->
        (match n.kind with
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
        | Ideal _ -> ()
        | _ -> Hashtbl.add ranges ~key:n ~data:(NodeSet.singleton n) |> ignore);
        if Machine_node.is_multi_output n then
          Graph.get_dependants g n
          |> List.filter ~f:(fun (n' : Machine_node.t) ->
              match n'.kind with
              | DProj _ -> Option.is_some (Machine_node.get_out_reg_mask g n' 0)
              | _ -> false)
          |> List.iter ~f:(fun proj ->
              Hashtbl.add ranges ~key:proj ~data:(NodeSet.singleton proj) |> ignore));
    let node_to_lrg = Hashtbl.create (module Machine_node) in
    Hashtbl.iter ranges ~f:(fun r ->
        (* def side constraints *)
        let reg_mask =
            Set.fold r ~init:Registers.Mask.all_and_stack ~f:(fun acc n ->
                match n.kind with
                | Ideal _ -> acc
                | _ ->
                    Registers.Mask.common acc
                      (Machine_node.get_out_reg_mask g n 0
                      |> Option.value ~default:Registers.Mask.empty))
        in
        if not (Registers.Mask.is_empty reg_mask) then (* use side constraints *)
          let reg_mask =
              Set.fold r ~init:reg_mask ~f:(fun acc n ->
                  let uses =
                      Graph.get_dependants g n
                      |> List.filter ~f:(fun use ->
                          match use.kind with
                          | Ideal _ -> false
                          | _ -> Hashtbl.mem ranges use)
                  in
                  List.fold uses ~init:acc ~f:(fun acc use ->
                      let i, _ =
                          List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ n' ->
                              match n' with
                              | None -> false
                              | Some n' -> Machine_node.equal n n')
                      in
                      match Machine_node.get_in_reg_mask g use (i - 1) with
                      | Some m -> Registers.Mask.common acc m
                      | None -> acc))
          in
          let lrg : Range.t = { reg_mask; nodes = r } in
          Set.iter r ~f:(fun n -> Hashtbl.set node_to_lrg ~key:n ~data:lrg));
    node_to_lrg

let print_lrgs node_to_lrg =
    Hashtbl.data node_to_lrg |> RangeSet.of_list |> RangeSet.show |> Printf.printf "%s\n"

type basic_block = {
    head : Machine_node.t;
    nodes : Machine_node.t list;
  }

type reg_assignment = (Range.t, Registers.loc) Hashtbl.t

type allocation_failure = {
    failed_ranges : Range.t list;
    self_conflicts : (Range.t, NodeSet.t) Hashtbl.t;
  }

module InterferenceGraph : sig
  type t

  val build :
    (Machine_node.t, 'a) Graph.t ->
    Machine_node.t list ->
    (Machine_node.t, Range.t) Hashtbl.t ->
    (t, allocation_failure) result

  val print : t -> unit
  val color : t -> (Machine_node.t, 'a) Graph.t -> (reg_assignment, allocation_failure) result
end = struct
  type t = (Range.t, RangeSet.t) Hashtbl.t

  let add_to_ifg ifg r r' =
      Hashtbl.update ifg r ~f:(function
        | None -> RangeSet.singleton r'
        | Some s -> Set.add s r');
      Hashtbl.update ifg r' ~f:(function
        | None -> RangeSet.singleton r
        | Some s -> Set.add s r)

  let mark_need_split tbl r = Hash_set.add tbl r

  let print ifg =
      Printf.printf "graph G {\n";
      Hashtbl.iteri ifg ~f:(fun ~key:r ~data:neighbors ->
          let r_id = Int.abs (Range.hash r) in
          Printf.printf "  r%d [label=\"%s\"];\n" r_id (String.escaped (Range.show r));
          Set.iter neighbors ~f:(fun r' ->
              if Range.compare r r' < 0 then
                Printf.printf "  r%d -- r%d;\n" r_id (Int.abs (Range.hash r'))));
      Printf.printf "}\n"

  let build g program node_to_lrg =
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
      let update_bb_outs self_conflicts bb (actives : (Range.t, Machine_node.t) Hashtbl.t) =
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
                      Hashtbl.find_or_add bb_outs prec ~default:(fun _ ->
                          Hashtbl.create (module Range))
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
                            Hashtbl.update self_conflicts lrg ~f:(function
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
          | Some prior ->
              if Machine_node.equal n prior then
                None
              else
                Some prior
          | None -> None
      in
      let check_kills need_splits (actives : (Range.t, 'a) Base.Hashtbl.t) n =
          match Machine_node.get_register_kills n with
          | None -> ()
          | Some kills ->
              Hashtbl.iter_keys actives ~f:(fun r' ->
                  if Registers.Mask.are_overlapping kills r'.reg_mask then
                    let diff = Registers.Mask.diff r'.reg_mask kills in
                    if not (Registers.Mask.is_empty diff) then
                      r'.reg_mask <- diff
                    else
                      mark_need_split need_splits r')
      in
      let do_node ifg need_splits self_conflicts actives node =
          (match Hashtbl.find node_to_lrg node with
          | None -> () (* not every node is in a range, e.g jmp nodes (if) *)
          | Some (range : Range.t) ->
              Hashtbl.add ifg ~key:range ~data:RangeSet.empty |> ignore;
              (* check def side conflict. eg. if there is already a def y for the lrg and node != y then conflict *)
              (match check_self_conflict actives range node with
              | Some other ->
                  Hashtbl.update self_conflicts range ~f:(function
                    | None -> NodeSet.of_list [ node; other ]
                    | Some s ->
                        let s = Set.add s node in
                        Set.add s other)
              | None -> ());
              (* remove the def *)
              Hashtbl.remove actives range;

              (* interfere with other live ranges *)
              Hashtbl.iter_keys actives ~f:(fun r' ->
                  if not (Registers.Mask.are_disjoint range.reg_mask r'.reg_mask) then
                    match
                      Machine_node.get_out_reg_mask g node 0
                    with
                    | Some mask when Registers.Mask.length mask = 1 ->
                        let reg = Registers.Mask.choose mask |> Option.value_exn in
                        let new_mask = Registers.Mask.remove r'.reg_mask reg in
                        r'.reg_mask <- new_mask;
                        if Registers.Mask.is_empty new_mask then
                          mark_need_split need_splits r'
                    | _ -> add_to_ifg ifg range r')
              (* check ranges that can only use a single register, in this case the other range should make place for it *));
          (* process registers kills *)
          check_kills need_splits actives node;

          (* add the nodes it uses *)
          Graph.get_dependencies g node
          |> List.tl
          |> Option.value ~default:[]
          |> List.iter ~f:(function
            | None -> ()
            | Some dep -> (
                match Hashtbl.find node_to_lrg dep with
                | None -> ()
                | Some range' ->
                    (match check_self_conflict actives range' dep with
                    | Some other ->
                        Hashtbl.update self_conflicts range' ~f:(function
                          | None -> NodeSet.of_list [ dep; other ]
                          | Some s ->
                              let s = Set.add s dep in
                              Set.add s other)
                    | None -> ());
                    Hashtbl.set actives ~key:range' ~data:dep))
      in
      let do_block ifg need_splits self_conflicts (bb : basic_block) =
          (* lrg -> bb where the lrg is defined *)
          let actives = get_actives bb in
          List.rev bb.nodes
          |> List.filter ~f:(fun n ->
              match n.kind with
              | Ideal _ -> false
              | _ -> true)
          |> List.iter ~f:(fun node ->
              if Machine_node.is_multi_output node then
                Graph.get_dependants g node
                |> List.filter ~f:(fun n ->
                    match n.kind with
                    | DProj _ -> true
                    | _ -> false)
                |> List.iter ~f:(do_node ifg need_splits self_conflicts actives);
              do_node ifg need_splits self_conflicts actives node);
          update_bb_outs self_conflicts bb actives
      in
      let ifg = Hashtbl.create (module Range) in
      let need_splits = Hash_set.create (module Range) in
      let self_conflicts = Hashtbl.create (module Range) in
      let queue = Queue.create () in
      List.iter program ~f:(fun l ->
          (* we want the blocks in reverse order *)
          let bb = { head = List.hd_exn l; nodes = List.tl l |> Option.value ~default:[] } in
          Queue.enqueue_front queue bb);
      while not (Queue.is_empty queue) do
        let bb = Queue.dequeue_exn queue in
        let changed = do_block ifg need_splits self_conflicts bb in
        Queue.enqueue_all queue changed
      done;
      if Hash_set.is_empty need_splits && Hashtbl.is_empty self_conflicts then
        Ok ifg
      else
        (* print ifg; *)
        (* Printf.printf "\nNeed Splits:\n"; *)
        (* Hash_set.iter need_splits ~f:(fun r -> Printf.printf "  %s\n" (Range.show r)); *)
        (* Printf.printf "\nSelf Conflicts:\n"; *)
        (* Hashtbl.iteri self_conflicts ~f:(fun ~key:r ~data:nodes -> *)
        (*     Printf.printf "  %s -> %s\n" (Range.show r) (NodeSet.show nodes)); *)
        Error { failed_ranges = Hash_set.to_list need_splits; self_conflicts }

  let degree ifg r =
      match Hashtbl.find ifg r with
      | None -> 0
      | Some l -> Set.length l

  let compare_trival_lrgs (r : Range.t) (r' : Range.t) =
      let l = Registers.Mask.length r.reg_mask in
      let l' = Registers.Mask.length r'.reg_mask in
      (* prefer lrg that only has a single choice *)

      if l = 1 && l' <> 1 then
        -1
      else if l <> 1 && l' = 1 then
        1
      else (* prefer large choice of registers *)
        Int.compare l l'

  let compare_non_trivial_lrgs g (r : Range.t) (r' : Range.t) =
      let risky_score g (r : Range.t) =
          (* if the range has not many neighbours it has an easier chance of "accidentally" getting colored despite being non trivial *)
          let base_score =
              1000
              (*- (Hashtbl.find_exn ifg r |> Set.length)*)
          in
          if Set.length r.nodes = 1 && Machine_node.is_cheap_to_clone (Set.choose_exn r.nodes) then
            let def = Set.choose_exn r.nodes in
            let use = Graph.get_dependants g def |> List.hd_exn in
            let cfg_def = Graph.get_dependency g def 0 |> Option.value_exn in
            let cfg_use = Graph.get_dependency g use 0 |> Option.value_exn in
            if not (Machine_node.equal cfg_def cfg_use) then
              (* single def cheaply clonable that is not close to its use is good to pick as it would just get cloned close to its use *)
              (* TODO: we shouldnt only check its first dependant *)
              100000
            else
              base_score
          else
            base_score
      in
      let score_1 = risky_score g r in
      let score_2 = risky_score g r' in
      Int.compare score_1 score_2

  let remove_from_graph ifg r =
      (* Printf.printf "Removing %s\n" (Range.show r); *)
      let neighbours = Hashtbl.find_exn ifg r in
      Set.iter neighbours ~f:(fun r' ->
          Hashtbl.change ifg r' ~f:(function
            | None -> None
            | Some s -> Some (Set.filter s ~f:(Fun.negate (Range.equal r)))));
      Hashtbl.remove ifg r;
      neighbours

  let simplify (ifg : t) g =
      let trivial_lrgs = Pairing_heap.create ~cmp:compare_trival_lrgs () in
      let non_trivial_lrgs = Hash_set.create (module Range) in
      Hashtbl.iter_keys ifg ~f:(fun (r : Range.t) ->
          let d = degree ifg r in
          if d < Registers.Mask.length r.reg_mask then
            (* Printf.printf "Adding %s\n" (Range.show r); *)
            Pairing_heap.add trivial_lrgs r
          else
            Hash_set.add non_trivial_lrgs r);

      let rec process acc =
          let visit_node r =
              let neighbours = remove_from_graph ifg r in
              Set.filter neighbours ~f:(fun r ->
                  let d = degree ifg r in
                  (* just now become trivial *)
                  d = Registers.Mask.length r.reg_mask - 1)
              |> Set.iter ~f:(fun r ->
                  (* Printf.printf "Adding to trivials: %s\n" (Range.show r); *)
                  Pairing_heap.add trivial_lrgs r;
                  Hash_set.remove non_trivial_lrgs r)
          in
          match Pairing_heap.pop trivial_lrgs with
          | Some r ->
              (* Printf.printf "Trivial: %s\n" (Range.show r); *)
              visit_node r;
              process (r :: acc)
          | None -> (
              let pop_first () =
                  match Hash_set.max_elt non_trivial_lrgs ~compare:(compare_non_trivial_lrgs g) with
                  | None -> None
                  | Some r ->
                      Hash_set.remove non_trivial_lrgs r;
                      Some r
              in
              match pop_first () with
              | None -> acc
              | Some r ->
                  (* Printf.printf "Non-Trivial: %s\n" (Range.show r); *)
                  visit_node r;
                  process (r :: acc))
      in
      process []

  let color ifg g =
      let ifg_copy = Hashtbl.copy ifg in
      let color_stack = simplify ifg_copy g in
      let reg_assoc : reg_assignment = Hashtbl.create (module Range) in
      let failed_ranges =
          List.fold color_stack ~init:[] ~f:(fun failed_ranges r ->
              let neighbours = Hashtbl.find_exn ifg r in
              let r_mask =
                  if Registers.Mask.mem r.reg_mask (Stack (-1)) then
                    (* If range can go on stack then add the first len(neighbours+1) stack slots,at worst we'll find one free slot among these *)
                    List.range 0 (Set.length neighbours + 1)
                    |> List.fold ~init:r.reg_mask ~f:(fun acc i -> Registers.Mask.add acc (Stack i))
                  else
                    r.reg_mask
              in
              let r_mask =
                  Set.fold neighbours ~init:r_mask ~f:(fun acc neighbour ->
                      match Hashtbl.find reg_assoc neighbour with
                      | None -> acc
                      | Some loc -> Registers.Mask.remove acc loc)
              in
              match Registers.Mask.choose r_mask with
              | None -> r :: failed_ranges
              | Some loc ->
                  assert (Poly.(loc <> Stack (-1)));
                  (* TODO: we could choose smarter here, see biasColor in Simple:IFG.java *)
                  (* Printf.printf "ASSIGNING: %s -> %s\n" (Registers.show_loc loc) (Range.show r); *)
                  Hashtbl.set reg_assoc ~key:r ~data:loc;
                  failed_ranges)
      in
      if List.is_empty failed_ranges then
        Ok reg_assoc
      else (* Printf.printf "Failed: %s\n" ([%derive.show: Range.t list] failed_ranges); *)
        Error { failed_ranges; self_conflicts = Hashtbl.create (module Range) }
end

let spill_node g n =
    let clone_node g (n : Machine_node.t) =
        let n' = { n with id = Machine_node.next_id () } in
        Graph.add_dependencies g n' (Graph.get_dependencies g n);
        n'
    in

    let copy_node g (n : Machine_node.t) =
        let n' : Machine_node.t =
            { id = Machine_node.next_id (); kind = Mov; ir_node = n.ir_node }
        in
        let cfg = Graph.get_dependency g n 0 in
        Graph.add_dependencies g n' [ cfg; Some n ];
        n'
    in
    if Machine_node.is_cheap_to_clone n then clone_node g n else copy_node g n

let rec insert_before g program ~(before_this : Machine_node.t) ~(node_to_spill : Machine_node.t) =
    assert (
      List.mem
        (Graph.get_dependencies g before_this)
        (Some node_to_spill) ~equal:(Option.equal Machine_node.equal));

    let end_of_bb bb =
        List.fold_until program ~init:None
          ~f:(fun acc n ->
            let cfg = if Machine_node.is_blockhead n then Some n else Graph.get_dependency g n 0 in
            match cfg with
            | None -> Continue acc
            | Some cfg -> (
                match acc with
                | None -> if Machine_node.equal bb cfg then Continue (Some n) else Continue None
                | Some n' -> if Machine_node.equal bb cfg then Continue (Some n) else Stop (Some n')
                ))
          ~finish:(fun res ->
            match res with
            | Some n -> Some n
            | None -> failwithf "Couldn't find end of bb %s" (Machine_node.show bb) ())
        |> Option.value_exn
    in
    let bb =
        match before_this.kind with
        | Ideal Phi ->
            let idx =
                Graph.get_dependencies g before_this
                |> List.find_mapi_exn ~f:(fun i n ->
                    match n with
                    | None -> None
                    | Some n -> if Machine_node.equal n node_to_spill then Some i else None)
            in
            let region = Graph.get_dependency g before_this 0 |> Option.value_exn in
            Graph.get_dependency g region idx |> Option.value_exn
        | _ -> Graph.get_dependency g before_this 0 |> Option.value_exn
    in
    let target =
        match before_this.kind with
        | Ideal Phi -> end_of_bb bb
        | _ -> before_this
    in
    match program with
    | [] -> []
    | h :: t when Machine_node.equal h target -> (
        let n' = spill_node g node_to_spill in
        (* Printf.printf "INSERTING %s (%s) before %s\n" (Machine_node.show n') *)
        (* (Machine_node.show node_to_spill) (Machine_node.show before_this); *)
        Graph.set_dependency g n' (Some bb) 0;
        let dep_idx, _ =
            List.findi_exn (Graph.get_dependencies g before_this) ~f:(fun _ dep ->
                match dep with
                | None -> false
                | Some dep -> Machine_node.equal dep node_to_spill)
        in
        Graph.set_dependency g before_this (Some n') dep_idx;
        match before_this.kind with
        | Ideal Phi -> h :: n' :: t (* put it after the last node of the bb *)
        | _ -> n' :: h :: t)
    | h :: t -> h :: insert_before g t ~node_to_spill ~before_this

let rec duplicate_after g program node =
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
    | h :: t when Machine_node.equal h node ->
        let dependants = Graph.get_dependants g node in
        let n' = spill_node g node in
        (* Printf.printf "INSERTING %s after %s\n" (Machine_node.show n') (Machine_node.show node); *)
        dependants
        |> List.iter ~f:(fun use ->
            let dep_idx, _ =
                List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                    match dep with
                    | None -> false
                    | Some dep -> Machine_node.equal dep node)
            in
            Graph.set_dependency g use (Some n') dep_idx);
        h :: skip_phis t n'
    | h :: t -> h :: duplicate_after g t node

let rec loop_depth g (node : Machine_node.t) =
    match node.kind with
    | Ideal Start -> 0
    | Ideal Stop -> 0
    | FunctionProlog _ -> 0
    | Ideal Loop ->
        let ld = 1 + loop_depth g (Loop_node.get_entry_edge g node) in
        ld
    | Ideal (CProj _) ->
        let if_node = Graph.get_dependency g node 0 |> Option.value_exn in
        let potential_loop = Graph.get_dependency g node 0 |> Option.value_exn in
        if Poly.equal potential_loop.kind (Ideal Loop) then
          let backedge = Loop_node.get_back_edge g potential_loop in
          if Machine_node.equal backedge node then
            loop_depth g if_node
          else
            loop_depth g if_node - 1
        else
          loop_depth g if_node
    | Ideal Region -> loop_depth g (Graph.get_dependency g node 1 |> Option.value_exn)
    | _ -> loop_depth g (Graph.get_dependency g node 0 |> Option.value_exn)

let split_self_conflict g program (self_conflicting_nodes : NodeSet.t) =
    Set.fold self_conflicting_nodes ~init:program ~f:(fun program n ->
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
                      && loop_depth g def_cfg > loop_depth g cfg
                    then
                      program
                    else
                      insert_before g program ~node_to_spill:n ~before_this:use
                | _
                  when Machine_node.is_two_address use
                       && Machine_node.equal (Graph.get_dependency g use 1 |> Option.value_exn) n ->
                    insert_before g program ~node_to_spill:n ~before_this:use
                | _ -> program)
        in
        match n.kind with
        | Ideal Phi ->
            let backedge = Loop_node.get_entry_edge g n in
            let program = duplicate_after g program n in
            insert_before g program ~node_to_spill:backedge ~before_this:n
        | _ when Machine_node.is_two_address n ->
            let dep = Graph.get_dependency g n 1 |> Option.value_exn in
            insert_before g program ~node_to_spill:dep ~before_this:n
        | _ -> program)

let split_empty_mask g program (lrg : Range.t) =
    let single_reg_defs =
        Set.filter lrg.nodes ~f:(fun n ->
            let num_regs =
                match Machine_node.get_out_reg_mask g n 0 with
                | Some mask -> Registers.Mask.length mask
                | None -> 0
            in
            num_regs = 1)
    in
    let single_reg_uses =
        Set.fold lrg.nodes ~init:[] ~f:(fun acc n ->
            let single_reg_uses =
                List.filter_map (Graph.get_dependants g n) ~f:(fun use ->
                    (* FIXME: this is not correct when a node takes in another node in more than one positions, e.g. div x,x  *)
                    let dep_idx, _ =
                        List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                            match dep with
                            | None -> false
                            | Some dep -> Machine_node.equal dep n)
                    in
                    let num_regs =
                        match Machine_node.get_in_reg_mask g use (dep_idx - 1) with
                        | Some mask -> Registers.Mask.length mask
                        | None -> 0
                    in
                    if num_regs = 1 then Some (n, use) else None)
            in
            single_reg_uses @ acc)
    in
    let single_reg_def_count = Set.length single_reg_defs in
    let single_reg_use_count = List.length single_reg_uses in
    if
      single_reg_def_count <= 1
      && single_reg_use_count <= 1
      && single_reg_def_count + single_reg_use_count > 0
    then
      let program =
          if Set.length single_reg_defs = 1 then
            duplicate_after g program (Set.choose_exn single_reg_defs)
          else
            program
      in
      let program =
          match single_reg_uses with
          | [ (def, use) ] -> insert_before g program ~before_this:use ~node_to_spill:def
          | _ -> program
      in
      program
    else if Set.length lrg.nodes = 1 && single_reg_def_count <= 1 && single_reg_use_count >= 2 then
      let program =
          let def = Set.choose_exn lrg.nodes in
          (* TODO: group uses into register classes instead of splitting before each use *)
          List.fold (Graph.get_dependants g def) ~init:program ~f:(fun program use ->
              insert_before g program ~before_this:use ~node_to_spill:def)
      in
      program
    else
      program

let ldepth g (n : Machine_node.t) (cfg : Machine_node.t) =
    ignore n;
    let d = loop_depth g cfg in
    d

let split_by_loop g program (lrg : Range.t) =
    let min_d, max_d =
        Set.fold lrg.nodes ~init:(Int.max_value, Int.min_value) ~f:(fun (min_d, max_d) def ->
            let d_def = ldepth g def (Graph.get_dependency g def 0 |> Option.value_exn) in
            let min_phi, max_phi =
                if Poly.equal def.kind (Ideal Phi) then
                  let region = Graph.get_dependency g def 0 |> Option.value_exn in
                  let cfgs =
                      Graph.get_dependencies g region
                      |> List.tl_exn
                      |> List.map ~f:(fun o -> Option.value_exn o)
                  in
                  let inputs =
                      Graph.get_dependencies g def
                      |> List.tl_exn
                      |> List.map ~f:(fun o -> Option.value_exn o)
                  in
                  List.zip_exn cfgs inputs
                  |> List.fold ~init:(Int.max_value, Int.min_value)
                       ~f:(fun (min_d, max_d) (cfg, input) ->
                         let d = ldepth g input cfg in
                         (min d min_d, max d max_d))
                else
                  (Int.max_value, Int.min_value)
            in
            let min_use, max_use =
                List.fold (Graph.get_dependants g def) ~init:(Int.max_value, Int.min_value)
                  ~f:(fun (min_d, max_d) use ->
                    match use.kind with
                    | Ideal Phi ->
                        (* Phi nodes are already accounted for outside this
                           loop since if a phi node is connected to the lrg as
                           a use then it is also in the lrg as a def since phi
                           nodes merge lrgs of their inputs and themself. See
                           build_live_ranges *)
                        (min_d, max_d)
                    | _ ->
                        let d = ldepth g use (Graph.get_dependency g use 0 |> Option.value_exn) in
                        (min d min_d, max d max_d))
            in
            ( min d_def (min min_phi (min min_use min_d)),
              max d_def (max max_phi (max max_use max_d)) ))
    in
    let all =
        Set.to_list lrg.nodes
        |> List.map ~f:(fun n -> NodeSet.of_list (n :: Graph.get_dependants g n))
        |> NodeSet.union_list
    in
    Set.fold all ~init:program ~f:(fun program n ->
        let program =
            if Set.mem lrg.nodes n then
              let cfg = Graph.get_dependency g n 0 |> Option.value_exn in
              let single_user_split_adjacent =
                  match Graph.get_dependants g n with
                  | [ x ] when Poly.equal x.kind Mov -> true
                  | _ -> false
              in
              let program =
                  if
                    (min_d = max_d || loop_depth g cfg <= min_d)
                    && (not (Machine_node.is_cheap_to_clone n))
                    && not (false && single_user_split_adjacent)
                  then
                    duplicate_after g program n
                  else
                    program
              in
              program
            else
              program
        in
        match n.kind with
        | Ideal Phi ->
            let cfg = Graph.get_dependency g n 0 |> Option.value_exn in
            let region = Graph.get_dependency g n 0 |> Option.value_exn in
            let cfgs =
                Graph.get_dependencies g region
                |> List.tl_exn
                |> List.map ~f:(fun o -> Option.value_exn o)
            in
            let inputs =
                Graph.get_dependencies g n
                |> List.tl_exn
                |> List.map ~f:(fun o -> Option.value_exn o)
            in
            List.zip_exn cfgs inputs
            |> List.foldi ~init:program ~f:(fun i program (cfg_in, input) ->
                if
                  (not (Poly.equal input.kind Mov))
                  && (min_d = max_d || loop_depth g cfg_in <= min_d)
                  && not
                       (Poly.equal cfg.kind (Ideal Loop)
                       && i = 1
                       && Poly.equal input.kind (Ideal Phi)
                       && Machine_node.equal cfg_in cfg)
                then
                  insert_before g program ~before_this:n ~node_to_spill:input
                else
                  program)
        | _ ->
            List.filter_map (Graph.get_dependencies g n) ~f:(function
              | None -> None
              | Some n -> if Set.mem lrg.nodes n then Some n else None)
            |> List.fold ~init:program ~f:(fun program use ->
                let cfg = Graph.get_dependency g n 0 |> Option.value_exn in
                if min_d = max_d || Machine_node.is_cheap_to_clone use || loop_depth g cfg <= min_d
                then
                  insert_before g program ~before_this:n ~node_to_spill:use
                else
                  program))

let split g program (lrg : Range.t) =
    (* TODO: actually calculate this *)
    let single_reg_use_count = 0 in
    if Registers.Mask.is_empty lrg.reg_mask && (Set.length lrg.nodes = 1 || single_reg_use_count = 1)
    then
      split_empty_mask g program lrg
    else
      split_by_loop g program lrg

let cleanup_mov g n reg_assign =
    let dep = Graph.get_dependency g n 1 |> Option.value_exn in
    let dst_loc = Hashtbl.find_exn reg_assign n in
    let src_loc = Hashtbl.find_exn reg_assign dep in
    if Poly.equal src_loc dst_loc then (
      Graph.replace_node_with g n dep;
      true)
    else
      false

let rec post_alloc_cleanup g (program : Machine_node.t list) reg_assign =
    match program with
    | [] -> []
    | h :: t -> (
        match h.kind with
        | Mov ->
            if cleanup_mov g h reg_assign then (
              Hashtbl.remove reg_assign h;
              post_alloc_cleanup g t reg_assign)
            else
              h :: post_alloc_cleanup g t reg_assign
        | _ -> h :: post_alloc_cleanup g t reg_assign)

let allocate g program =
    let ( let* ) = Stdlib.Result.bind in
    let rec loop program round =
        (* Printf.printf "\n\n ================= ROUND %d ======================\n\n" round; *)
        let attempt () =
            (* Ir_printer.to_dot_machine g |> Printf.printf "\n\n%s\n"; *)
            let node_to_lrg = build_live_ranges g program in
            (* print_lrgs node_to_lrg; *)
            let* ifg = InterferenceGraph.build g program node_to_lrg in
            (* InterferenceGraph.print ifg; *)
            let* coloring = InterferenceGraph.color ifg g in
            Ok coloring
        in
        if round > 7 then
          failwith "This should've finished by now"
        else
          match
            attempt ()
          with
          | Ok reg_assoc ->
              (* Printf.printf "Register allocation done in %d rounds\n" round; *)
              (program, reg_assoc)
          | Error { failed_ranges; self_conflicts } ->
              (* TODO: perhaps sort failed ranges to make sure we are deterministic *)
              let new_program =
                  Hashtbl.fold self_conflicts ~init:program
                    ~f:(fun ~key:_ ~data:self_conflicting_nodes program ->
                      split_self_conflict g program self_conflicting_nodes)
              in
              let new_program =
                  List.fold failed_ranges ~init:new_program ~f:(fun program lrg ->
                      split g program lrg)
              in
              loop new_program (round + 1)
    in
    let program, lrg_to_reg = loop program 1 in
    (* Hashtbl.iteri lrg_to_reg ~f:(fun ~key ~data -> *)
    (* Printf.printf "%s : %s\n\n" (Registers.show_loc data) (Range.show key)); *)
    let reg_assign = Hashtbl.create (module Machine_node) in
    Hashtbl.iteri lrg_to_reg ~f:(fun ~key:range ~data:reg ->
        Set.iter range.nodes ~f:(fun n -> Hashtbl.set reg_assign ~key:n ~data:reg));
    let program = post_alloc_cleanup g program reg_assign in
    (program, reg_assign)
