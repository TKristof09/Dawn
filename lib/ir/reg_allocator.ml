open Core
module Graph = Machine_node.G

module NodeSet = struct
  include Set.Make_plain (struct
    include Machine_node.Any
    include Comparator.Make (Machine_node.Any)
  end)

  let show s = Set.to_list s |> [%derive.show: Machine_node.any list] |> Printf.sprintf "%s"
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Range = struct
  type t = {
      mutable reg_mask : Registers.Mask.t;
      mutable nodes : NodeSet.t;
      single_reg_uses : NodeSet.t;
    }
  [@@deriving sexp_of]

  let show t =
      Printf.sprintf "%s\n\t\t--->%s" (NodeSet.show t.nodes) (Registers.Mask.show t.reg_mask)

  let pp fmt r = Format.fprintf fmt "%s" (show r)
  let compare t t' = Set.compare_direct t.nodes t'.nodes
  let equal t t' = Set.equal t.nodes t'.nodes

  let hash t =
      let state = Hash.alloc () in
      Set.fold t.nodes ~init:state ~f:(fun s (AnyNode n) -> Hash.fold_int s (Machine_node.hash n))
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

type allocation_failure = {
    failed_ranges : Range.t list;
    self_conflicts : (Range.t, NodeSet.t) Hashtbl.t;
  }

let build_live_ranges (g : Graph.readonly Graph.t) program =
    let ranges = Hashtbl.create (module Machine_node.Any) in
    List.iter program ~f:(fun (Machine_node.AnyNode n) ->
        let (Node.AnyNode ir_node) = n.ir_node in
        (match n.kind with
        | Ideal Phi when not (Types.equal ir_node.typ Memory) ->
            (* merge all depedencies into same live range except for the control depedency, we don't care about that *)
            let { Machine_node.phi_inputs } = Graph.get_dependencies_exn g n in
            let merged =
                NodeSet.union_list
                  (List.filter_opt phi_inputs
                  |> List.filter_map ~f:(fun (AnyNode dep) ->
                      match dep.kind with
                      | Ideal Phi ->
                          Some
                            (Hashtbl.find_or_add ranges (AnyNode dep) ~default:(fun () ->
                                 NodeSet.singleton (AnyNode dep)))
                      | Ideal _ -> None
                      | _ ->
                          Some
                            (Hashtbl.find_or_add ranges (AnyNode dep) ~default:(fun () ->
                                 NodeSet.singleton (AnyNode dep)))))
            in
            let this_lrg =
                Hashtbl.find ranges (AnyNode n)
                |> Option.value ~default:(NodeSet.singleton (AnyNode n))
            in
            let merged = Set.union merged this_lrg in
            Set.iter merged ~f:(fun n -> Hashtbl.set ranges ~key:n ~data:merged)
        | _ when Machine_node.is_two_address n ->
            let input1 = Graph.get_dependencies_list g n |> List.hd_exn |> Option.value_exn in
            let input_lrg =
                Hashtbl.find_or_add ranges input1 ~default:(fun () -> NodeSet.singleton input1)
            in
            let this_lrg =
                Hashtbl.find ranges (AnyNode n)
                |> Option.value ~default:(NodeSet.singleton (AnyNode n))
            in
            let merged = Set.union input_lrg this_lrg in
            Set.iter merged ~f:(fun n -> Hashtbl.set ranges ~key:n ~data:merged)
        | Ideal _ -> ()
        | _ -> Hashtbl.add ranges ~key:(AnyNode n) ~data:(NodeSet.singleton (AnyNode n)) |> ignore);
        if Machine_node.is_multi_output n then
          Graph.get_dependants g n
          |> List.filter ~f:(fun (AnyNode n') ->
              match n'.kind with
              | DProj _ -> Option.is_some (Machine_node.get_out_reg_mask g n' 0)
              | _ -> false)
          |> List.iter ~f:(fun proj ->
              Hashtbl.add ranges ~key:proj ~data:(NodeSet.singleton proj) |> ignore));
    let node_to_lrg = Hashtbl.create (module Machine_node.Any) in
    let failed_ranges = Hash_set.create (module Range) in
    Hashtbl.iter ranges ~f:(fun r ->
        (* def side constraints *)
        let reg_mask =
            Set.fold r ~init:Registers.Mask.all_and_stack ~f:(fun acc (AnyNode n) ->
                match n.kind with
                | Ideal _ -> acc
                | _ ->
                    Registers.Mask.common acc
                      (Machine_node.get_out_reg_mask g n 0
                      |> Option.value ~default:Registers.Mask.empty))
        in
        (* use side constraints *)
        if not (Registers.Mask.is_empty reg_mask) then (
          let reg_mask, single_reg_uses =
              Set.fold r ~init:(reg_mask, NodeSet.empty)
                ~f:(fun (reg_mask, single_reg_use_count) (AnyNode n) ->
                  let uses =
                      Graph.get_dependants g n
                      |> List.filter ~f:(fun (AnyNode use) ->
                          match use.kind with
                          | Ideal _ -> false
                          | _ -> Hashtbl.mem ranges (AnyNode use))
                  in
                  List.fold uses ~init:(reg_mask, single_reg_use_count)
                    ~f:(fun (reg_mask, single_reg_uses) (AnyNode use) ->
                      let i, _ =
                          List.findi_exn (Graph.get_dependencies_list g use) ~f:(fun _ n' ->
                              match n' with
                              | None -> false
                              | Some (AnyNode n') -> Machine_node.equal n n')
                      in
                      match Machine_node.get_in_reg_mask g use i with
                      | Some m ->
                          let single_reg_uses =
                              if Registers.Mask.length m = 1 then
                                Set.add single_reg_uses (AnyNode use)
                              else
                                single_reg_uses
                          in
                          (Registers.Mask.common reg_mask m, single_reg_uses)
                      | None -> (reg_mask, single_reg_uses)))
          in
          let lrg : Range.t = { reg_mask; nodes = r; single_reg_uses } in
          if Registers.Mask.is_empty lrg.reg_mask then
            Hash_set.add failed_ranges lrg;
          Set.iter r ~f:(fun n -> Hashtbl.set node_to_lrg ~key:n ~data:lrg)));
    if Hash_set.is_empty failed_ranges then
      Ok node_to_lrg
    else
      Error
        {
          failed_ranges = Hash_set.to_list failed_ranges;
          self_conflicts = Hashtbl.create (module Range);
        }

let pp_lrgs fmt node_to_lrg =
    Format.fprintf fmt "%a" RangeSet.pp (Hashtbl.data node_to_lrg |> RangeSet.of_list)

type basic_block = {
    head : Machine_node.any;
    nodes : Machine_node.any list;
  }

type reg_assignment = (Range.t, Registers.loc) Hashtbl.t * int

module InterferenceGraph : sig
  type t

  val build :
    Machine_node.G.readonly Machine_node.G.t ->
    Machine_node.any list ->
    (Machine_node.any, Range.t) Hashtbl.t ->
    (t, allocation_failure) result

  val color :
    t -> Machine_node.G.readonly Machine_node.G.t -> (reg_assignment, allocation_failure) result

  val pp : Format.formatter -> t -> unit
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

  let pp pp_f ifg =
      Format.fprintf pp_f "graph G {\n%a\n}"
        (fun fmt () ->
          Hashtbl.iteri ifg ~f:(fun ~key:r ~data:neighbors ->
              let r_id = Int.abs (Range.hash r) in
              Format.fprintf fmt "  r%d [label=\"%a\"];@," r_id Range.pp r;
              Set.iter neighbors ~f:(fun r' ->
                  if Range.compare r r' < 0 then
                    Format.fprintf fmt "  r%d -- r%d;@," r_id (Int.abs (Range.hash r')))))
        ()

  let build g program node_to_lrg =
      let program =
          List.group program ~break:(fun _ (Machine_node.AnyNode n) -> Machine_node.is_blockhead n)
      in
      let bb_outs = Hashtbl.create (module Machine_node.Any) in
      let bbs =
          match
            Hashtbl.create_mapped
              (module Machine_node.Any)
              ~get_key:List.hd_exn
              ~get_data:(fun l ->
                { head = List.hd_exn l; nodes = List.tl l |> Option.value ~default:[] })
              program
          with
          | `Ok m -> m
          | `Duplicate_keys _ -> assert false
      in
      let update_bb_outs self_conflicts bb actives =
          let rec get_blockhead (Machine_node.AnyNode n) =
              if Machine_node.is_blockhead n then
                Machine_node.AnyNode n
              else
                Graph.get_ctrl_exn g n |> get_blockhead
          in

          let (AnyNode head) = bb.head in
          let precs =
              match head.kind with
              | Ideal Region ->
                  let { Machine_node.ctrl_inputs } = Graph.get_dependencies_exn g head in
                  ctrl_inputs
              | Ideal Loop ->
                  let { Machine_node.entry; backedge } = Graph.get_dependencies_exn g head in
                  [ Some entry; Some backedge ]
              | Ideal Start -> []
              | _ -> [ Graph.get_ctrl g head ]
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
                  Hashtbl.fold actives ~init:false
                    ~f:(fun ~key:lrg ~data:(Machine_node.AnyNode n) acc ->
                      let (AnyNode def) =
                          match n.kind with
                          | Ideal Phi ->
                              let (AnyNode ctrl) = Graph.get_ctrl_exn g n in
                              let { Machine_node.phi_inputs } = Graph.get_dependencies_exn g n in
                              if Machine_node.equal ctrl head then
                                List.nth_exn phi_inputs i |> Option.value_exn
                              else
                                AnyNode n
                          | _ -> AnyNode n
                      in
                      match Hashtbl.find prec_live_outs lrg with
                      | None ->
                          Hashtbl.set prec_live_outs ~key:lrg ~data:(AnyNode def);
                          true
                      | Some (Machine_node.AnyNode n') ->
                          if Machine_node.equal def n' then
                            ()
                          else
                            Hashtbl.update self_conflicts lrg ~f:(function
                              | None -> NodeSet.of_list [ AnyNode def; AnyNode n' ]
                              | Some s ->
                                  let s = Set.add s (AnyNode def) in
                                  Set.add s (AnyNode n'));
                          acc))
          |> List.filter_opt
          |> List.map ~f:(Hashtbl.find_exn bbs)
      in
      let get_actives bb =
          match Hashtbl.find bb_outs bb.head with
          | None -> Hashtbl.Poly.create ()
          | Some outs -> Hashtbl.copy outs
      in
      let check_self_conflict actives range (Machine_node.AnyNode n) =
          match Hashtbl.find actives range with
          | Some (Machine_node.AnyNode prior) ->
              if Machine_node.equal n prior then
                None
              else
                Some (Machine_node.AnyNode prior)
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
                    else (
                      r'.reg_mask <- diff;
                      [%log.debug "Killed %a by %a" Range.pp r' Machine_node.pp n];
                      mark_need_split need_splits r'))
      in
      let do_node ifg need_splits self_conflicts actives node =
          let (Machine_node.AnyNode node_unwrapped) = node in
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
                      Machine_node.get_out_reg_mask g node_unwrapped 0
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
          check_kills need_splits actives node_unwrapped;

          (* add the nodes it uses *)
          Graph.get_dependencies_list g node_unwrapped
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
          |> List.filter ~f:(fun (AnyNode n) ->
              match n.kind with
              | Ideal _ -> false
              | _ -> true)
          |> List.iter ~f:(fun (AnyNode node) ->
              if Machine_node.is_multi_output node then
                Graph.get_dependants g node
                |> List.filter ~f:(fun (AnyNode n) ->
                    match n.kind with
                    | DProj _ -> true
                    | _ -> false)
                |> List.iter ~f:(do_node ifg need_splits self_conflicts actives);
              do_node ifg need_splits self_conflicts actives (AnyNode node));
          let (AnyNode bb_head) = bb.head in
          (match bb_head.kind with
          | Ideal _ -> ()
          | _ -> check_kills need_splits actives bb_head);
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
      else (
        [%log.debug "%a" pp ifg];

        [%log.debug
            "Need splits:\n%a"
              (fun fmt () ->
                Hash_set.iter need_splits ~f:(fun r -> Format.fprintf fmt "  %a@," Range.pp r))
              ()];

        [%log.debug
            "Self conflicts:\n%a"
              (fun fmt () ->
                Hashtbl.iteri self_conflicts ~f:(fun ~key:r ~data:nodes ->
                    Format.fprintf fmt "  %a -> %a@," Range.pp r NodeSet.pp nodes))
              ()];
        Error { failed_ranges = Hash_set.to_list need_splits; self_conflicts })

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
      else if l = l' then (* make it deterministic *)
        let (AnyNode m1) = Set.min_elt_exn r.nodes in
        let (AnyNode m2) = Set.min_elt_exn r'.nodes in
        Int.compare m1.id m2.id
      else (* prefer large choice of registers *)
        Int.compare l l'

  let compare_non_trivial_lrgs original_ifg g (r : Range.t) (r' : Range.t) =
      let risky_score original_ifg g (r : Range.t) =
          let base_score =
              let (AnyNode n) = Set.choose_exn r.nodes in
              match n.kind with
              (* prefer callee save for risky pick as they cover big area (entire function) *)
              | Machine_node.CalleeSave _ -> 100000 - 1
              | Mov when Set.length r.nodes = 1 && Registers.Mask.mem r.reg_mask (Stack (-1)) ->
                  (* Choose movs first as these are created when spilling an
                     lrg and these are the ones that we can place on the stack
                     too. This only applies to the spill part (which has the
                     stack slot in the lrg's reg mask) and not to the reload
                     which has to go in some more constrained register *)
                  100_000
              | _ ->
                  (* prefer to split ranges that have a big area so they free
                     up stuff for longer (we approximate big area by large
                     degree in original graph but it would be better to store
                     in the ranges where they start and where they end ) *)
                  1000 + (Hashtbl.find_exn original_ifg r |> Set.length)
          in
          let (AnyNode choice) = Set.choose_exn r.nodes in
          if Set.length r.nodes = 1 && Machine_node.is_cheap_to_clone choice then
            let def = choice in
            let (AnyNode use) = Graph.get_dependants g def |> List.hd_exn in
            let (AnyNode cfg_def) = Graph.get_ctrl_exn g def in
            let (AnyNode cfg_use) = Graph.get_ctrl_exn g use in
            if not (Machine_node.equal cfg_def cfg_use) then
              (* single def cheaply clonable that is not close to its use is good to pick as it would just get cloned close to its use *)
              (* TODO: we shouldnt only check its first dependant *)
              100_000
            else
              base_score
          else
            base_score
      in
      let score_1 = risky_score original_ifg g r in
      let score_2 = risky_score original_ifg g r' in
      if score_1 = score_2 then (* make it deterministic *)
        let (AnyNode m1) = Set.min_elt_exn r.nodes in
        let (AnyNode m2) = Set.min_elt_exn r'.nodes in
        Int.compare m1.id m2.id
      else
        Int.compare score_1 score_2

  let remove_from_graph ifg r =
      let neighbours = Hashtbl.find_exn ifg r in
      Set.iter neighbours ~f:(fun r' ->
          Hashtbl.change ifg r' ~f:(function
            | None -> None
            | Some s -> Some (Set.filter s ~f:(Fun.negate (Range.equal r)))));
      Hashtbl.remove ifg r;
      neighbours

  let simplify (original_ifg : t) (ifg : t) g =
      let trivial_lrgs = Pairing_heap.create ~cmp:compare_trival_lrgs () in
      let non_trivial_lrgs = Hash_set.create (module Range) in
      Hashtbl.iter_keys ifg ~f:(fun (r : Range.t) ->
          let d = degree ifg r in
          if d < Registers.Mask.length r.reg_mask then
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
                  [%log.debug "Adding to trivials: %a" Range.pp r];
                  Pairing_heap.add trivial_lrgs r;
                  Hash_set.remove non_trivial_lrgs r)
          in
          match Pairing_heap.pop trivial_lrgs with
          | Some r ->
              [%log.debug "Trivial %a" Range.pp r];
              visit_node r;
              process (r :: acc)
          | None -> (
              let pop_first () =
                  match
                    Hash_set.max_elt non_trivial_lrgs
                      ~compare:(compare_non_trivial_lrgs original_ifg g)
                  with
                  | None -> None
                  | Some r ->
                      Hash_set.remove non_trivial_lrgs r;
                      Some r
              in
              match pop_first () with
              | None -> acc
              | Some r ->
                  [%log.debug "Non-trivial %a" Range.pp r];
                  visit_node r;
                  process (r :: acc))
      in
      process []

  let color ifg g =
      let ifg_copy = Hashtbl.copy ifg in
      let color_stack = simplify ifg ifg_copy g in
      let reg_assoc = Hashtbl.create (module Range) in
      let failed_ranges, max_stack_slot =
          List.fold color_stack ~init:([], 0) ~f:(fun (failed_ranges, max_stack_slot) r ->
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
              | None ->
                  [%log.debug "No available reg for %a" Range.pp r];
                  (r :: failed_ranges, max_stack_slot)
              | Some loc ->
                  assert (Poly.(loc <> Stack (-1)));
                  (* TODO: we could choose smarter here, see biasColor in Simple:IFG.java *)
                  [%log.debug "Assigning %a -> %a" Registers.pp_loc loc Range.pp r];
                  Hashtbl.set reg_assoc ~key:r ~data:loc;
                  let stack_slot =
                      match loc with
                      | Stack s -> s
                      | Reg _ -> 0
                  in
                  (failed_ranges, max stack_slot max_stack_slot))
      in
      if List.is_empty failed_ranges then
        Ok (reg_assoc, max_stack_slot)
      else (
        [%log.debug "Coloring failed for %a" (Format.pp_print_list Range.pp) failed_ranges];
        Error { failed_ranges; self_conflicts = Hashtbl.create (module Range) })
end

let spill_node g (lrg : Range.t) (Machine_node.AnyNode n) =
    assert (not @@ Machine_node.is_control_node n);

    let clone_node : type a b.
        Graph.readwrite Graph.t -> (a, b) Machine_node.t -> (a, unit) Machine_node.t =
       fun g n ->
        let n' = Machine_node.create_node n.kind n.ir_node in
        let (AnyNode cfg) = Graph.get_ctrl_exn g n in
        Graph.add_node g n' (Graph.get_dependencies_exn g n);
        Graph.set_ctrl g n' cfg;
        n'
    in

    let copy_node : type a b.
        Graph.readwrite Graph.t ->
        (a, b) Machine_node.t ->
        (Machine_node.unary, unit) Machine_node.t =
       fun g n ->
        let n' = Machine_node.create_node Mov n.ir_node in
        let (AnyNode cfg) =
            match n.kind with
            | DProj _ ->
                let { Machine_node.input = AnyNode input } = Graph.get_dependencies_exn g n in
                if Machine_node.is_blockhead input then
                  Machine_node.AnyNode input
                else
                  Graph.get_ctrl_exn g input
            | _ -> Graph.get_ctrl_exn g n
        in
        assert (Machine_node.is_blockhead cfg);
        Graph.add_node g n' { input = AnyNode n };
        Graph.set_ctrl g n' cfg;
        n'
    in
    if Machine_node.is_cheap_to_clone n then (
      let n' = Machine_node.AnyNode (clone_node g n) in
      lrg.nodes <- Set.add lrg.nodes n';
      n')
    else
      let n' = Machine_node.AnyNode (copy_node g n) in
      lrg.nodes <- Set.add lrg.nodes n';
      n'

let insert_before ?(skip = true) g program ~before_this ~node_to_spill lrg =
    let rec insert_aux g program ~before_this ~node_to_spill =
        let (Machine_node.AnyNode before_this_unwrapped) = before_this in
        let (Machine_node.AnyNode node_to_spill_unwrapped) = node_to_spill in
        assert (
          List.mem
            (Graph.get_dependencies_list g before_this_unwrapped)
            (Some node_to_spill)
            ~equal:
              (Option.equal (fun (Machine_node.AnyNode a) (AnyNode b) -> Machine_node.equal a b)));

        let end_of_bb bb =
            List.fold_until program ~init:None
              ~f:(fun acc (Machine_node.AnyNode n) ->
                let cfg =
                    match n.kind with
                    | DProj _ ->
                        let { Machine_node.input = AnyNode input } =
                            Graph.get_dependencies_exn g n
                        in
                        if Machine_node.is_blockhead input then
                          Some (Machine_node.AnyNode input)
                        else
                          Graph.get_ctrl g input
                    | _ ->
                        if Machine_node.is_blockhead n then Some (AnyNode n) else Graph.get_ctrl g n
                in
                match cfg with
                | None -> Continue acc
                | Some (AnyNode cfg) -> (
                    match acc with
                    | None ->
                        if Machine_node.equal bb cfg then
                          Continue (Some (Machine_node.AnyNode n))
                        else
                          Continue None
                    | Some n' ->
                        if Machine_node.equal bb cfg then
                          Continue (Some (AnyNode n))
                        else
                          Stop (Some n')))
              ~finish:(fun res ->
                match res with
                | Some n -> Some n
                | None -> failwithf "Couldn't find end of bb %s" (Machine_node.show bb) ())
            |> Option.value_exn
        in
        let (AnyNode bb) =
            match before_this_unwrapped.kind with
            | Ideal Phi ->
                let { Machine_node.phi_inputs } =
                    Machine_node.G.get_dependencies_exn g before_this_unwrapped
                in
                let (AnyNode region) = Machine_node.G.get_ctrl_exn g before_this_unwrapped in
                let ctrl_inputs =
                    match region.kind with
                    | Ideal Region ->
                        let { Machine_node.ctrl_inputs } =
                            Machine_node.G.get_dependencies_exn g region
                        in
                        ctrl_inputs
                    | Ideal Loop ->
                        let { Machine_node.entry; backedge } =
                            Machine_node.G.get_dependencies_exn g region
                        in
                        [ Some entry; Some backedge ]
                    | _ -> assert false
                in
                List.zip_exn ctrl_inputs phi_inputs
                |> List.find_map_exn ~f:(fun (ctrl, data) ->
                    match data with
                    | None -> None
                    | Some (AnyNode data) ->
                        if Machine_node.equal data node_to_spill_unwrapped then
                          Some (Option.value_exn ctrl)
                        else
                          None)
            | _ -> Graph.get_ctrl_exn g before_this_unwrapped
        in
        let (AnyNode target) =
            match before_this_unwrapped.kind with
            | Ideal Phi -> end_of_bb bb
            | _ -> AnyNode before_this_unwrapped
        in
        match program with
        | [] -> []
        | Machine_node.AnyNode h :: t when Machine_node.equal h target -> (
            let (AnyNode n') = spill_node g lrg node_to_spill in
            Graph.set_ctrl g n' bb;
            Graph.replace_input_unsafe g ~node:before_this_unwrapped ~from:node_to_spill
              ~to_:(AnyNode n');
            match before_this_unwrapped.kind with
            | Ideal Phi ->
                Machine_node.AnyNode h :: AnyNode n' :: t (* put it after the last node of the bb *)
            | _ -> AnyNode n' :: AnyNode h :: t)
        | h :: t -> h :: insert_aux g t ~node_to_spill ~before_this
    in

    let (Machine_node.AnyNode before_this_unwrapped) = before_this in
    let (Machine_node.AnyNode node_to_spill_unwrapped) = node_to_spill in
    (* if there is an intermediary split or clone already inserted we don't do anything *)
    if
      skip
      && not
         @@ List.mem
              (Graph.get_dependencies_list g before_this_unwrapped)
              (Some node_to_spill)
              ~equal:
                (Option.equal (fun (Machine_node.AnyNode a) (AnyNode b) -> Machine_node.equal a b))
    then
      match
        Graph.get_dependencies_list g before_this_unwrapped
        |> List.find ~f:(function
          | None -> false
          | Some (AnyNode n) -> (
              if Machine_node.is_cheap_to_clone node_to_spill_unwrapped then
                Machine_node.equal_kind n.kind node_to_spill_unwrapped.kind
                && List.equal
                     (Option.equal (fun (Machine_node.AnyNode a) (AnyNode b) ->
                          Machine_node.equal a b))
                     (Graph.get_dependencies_list g n)
                     (Graph.get_dependencies_list g node_to_spill_unwrapped)
              else
                match
                  n.kind
                with
                | Mov ->
                    List.mem (Graph.get_dependants g n) before_this
                      ~equal:(fun (Machine_node.AnyNode a) (AnyNode b) -> Machine_node.equal a b)
                | _ -> false))
      with
      | None -> assert false
      | Some _ -> program
    else
      insert_aux g program ~before_this ~node_to_spill

let rec duplicate_after g program lrg node =
    let rec skip_phis_and_callee_save l n =
        match l with
        | [] -> [ n ]
        | Machine_node.AnyNode h :: t -> (
            match h.kind with
            | Ideal Phi
            | CalleeSave _ ->
                Machine_node.AnyNode h :: skip_phis_and_callee_save t n
            | _ -> n :: l)
    in
    match program with
    | [] -> []
    | Machine_node.AnyNode h :: t when Machine_node.equal h node ->
        let dependants = Graph.get_dependants g node in
        let n' = spill_node g lrg (AnyNode node) in
        dependants
        |> List.iter ~f:(fun (AnyNode use) ->
            Graph.replace_input_unsafe g ~node:use ~from:(AnyNode node) ~to_:n');
        Machine_node.AnyNode h :: skip_phis_and_callee_save t n'
    | h :: t -> h :: duplicate_after g t lrg node

(* TODO: don't duplicate with scheduler.ml *)
let rec loop_depth g (Machine_node.AnyNode n) =
    match n.kind with
    | Ideal Loop ->
        let { Machine_node.entry; backedge = _ } = Machine_node.G.get_dependencies_exn g n in
        loop_depth g entry + 1
    | Ideal Start
    | FunctionProlog _ ->
        1
    | Ideal Region ->
        let { Machine_node.ctrl_inputs } = Machine_node.G.get_dependencies_exn g n in
        loop_depth g (List.hd_exn ctrl_inputs |> Option.value_exn)
    | Ideal (CProj 1) -> (
        let { Machine_node.input = AnyNode p } = Machine_node.G.get_dependencies_exn g n in
        match p.kind with
        | Jmp _ -> (
            let (AnyNode p) = Machine_node.G.get_ctrl_exn g p in
            match p.kind with
            | Ideal Loop ->
                let { Machine_node.entry; backedge = _ } =
                    Machine_node.G.get_dependencies_exn g p
                in
                loop_depth g entry
            | _ -> loop_depth g (Machine_node.G.get_ctrl_exn g n))
        | _ -> loop_depth g (Machine_node.G.get_ctrl_exn g n))
    | _ -> loop_depth g (Machine_node.G.get_ctrl_exn g n)

let split_self_conflict g program lrg (self_conflicting_nodes : NodeSet.t) =
    Set.fold self_conflicting_nodes ~init:program ~f:(fun program (AnyNode n) ->
        let program =
            List.fold (Graph.get_dependants g n) ~init:program ~f:(fun program (AnyNode use) ->
                match use.kind with
                | Ideal Phi ->
                    let (AnyNode cfg) = Graph.get_ctrl_exn g use in
                    let def_cfg = Graph.get_ctrl_exn g n in

                    let (AnyNode backedge) =
                        Machine_node.get_phi_backedge (Machine_node.G.readonly g) use
                        |> Option.value_exn
                    in
                    let is_backedge = Machine_node.equal backedge n in
                    let is_loop =
                        match cfg.kind with
                        | Ideal Loop -> true
                        | _ -> false
                    in
                    if is_backedge && is_loop && loop_depth g def_cfg > loop_depth g (AnyNode cfg)
                    then
                      program
                    else
                      insert_before g program lrg ~node_to_spill:(AnyNode n)
                        ~before_this:(AnyNode use)
                | _ when Machine_node.is_two_address use ->
                    let (AnyNode input1) =
                        Graph.get_dependencies_list g use |> List.hd_exn |> Option.value_exn
                    in
                    if Machine_node.equal input1 n then
                      insert_before g program lrg ~node_to_spill:(AnyNode n)
                        ~before_this:(AnyNode use)
                    else
                      program
                | _ -> program)
        in
        match n.kind with
        | Ideal Phi ->
            let (AnyNode backedge) =
                Machine_node.get_phi_backedge (Machine_node.G.readonly g) n |> Option.value_exn
            in
            let program = duplicate_after g program lrg n in
            insert_before g program lrg ~node_to_spill:(AnyNode backedge) ~before_this:(AnyNode n)
        | _ when Machine_node.is_two_address n ->
            let (AnyNode input1) =
                Graph.get_dependencies_list g n |> List.hd_exn |> Option.value_exn
            in
            insert_before g program lrg ~node_to_spill:(AnyNode input1) ~before_this:(AnyNode n)
        | _ -> program)

let split g program (lrg : Range.t) =
    Set.fold lrg.nodes ~init:program ~f:(fun prog (AnyNode n) ->
        (* create reloads before every use *)
        let prog =
            List.fold (Graph.get_dependants g n) ~init:prog ~f:(fun prog use ->
                insert_before g prog lrg ~before_this:use ~node_to_spill:(AnyNode n))
        in
        (* create spill, this makes every reload that we just created use the
           spill rather than the original n. Except if the node was
           rematerialised and not spilled *)
        if not (Machine_node.is_cheap_to_clone n) then
          duplicate_after g prog lrg n
        else
          prog)

let cleanup_mov g (n : (Machine_node.unary, 'a) Machine_node.t) reg_assign =
    let { Machine_node.input = AnyNode input } = Graph.get_dependencies_exn g n in
    let dst_loc = Hashtbl.find_exn reg_assign (Machine_node.AnyNode n) in
    let src_loc = Hashtbl.find_exn reg_assign (Machine_node.AnyNode input) in
    if Registers.equal_loc src_loc dst_loc then (
      Graph.replace_node_with_unsafe g ~from:(AnyNode n) ~to_:(AnyNode input);
      true)
    else
      false

let rec post_alloc_cleanup g program reg_assign =
    match program with
    | [] -> []
    | Machine_node.AnyNode h :: t -> (
        match h.kind with
        | Mov ->
            if cleanup_mov g h reg_assign then (
              Hashtbl.remove reg_assign (AnyNode h);
              post_alloc_cleanup g t reg_assign)
            else
              Machine_node.AnyNode h :: post_alloc_cleanup g t reg_assign
        | _ -> Machine_node.AnyNode h :: post_alloc_cleanup g t reg_assign)

type allocation_result = {
    max_stack_slot : int;
    reg_assoc : (Machine_node.any, Registers.loc) Core.Hashtbl.t;
  }

let allocate g program =
    let ( let* ) = Stdlib.Result.bind in
    let rec loop program round =
        [%log.debug "\n\n ================= ROUND %d ======================\n\n" round];
        let attempt g program =
            [%log.debug "\n%a" Ir_printer.pp_machine_linear (Graph.readonly g, program)];
            let* node_to_lrg = build_live_ranges (Graph.readonly g) program in
            [%log.debug "Build live ranges:\n%a" pp_lrgs node_to_lrg];
            let* ifg = InterferenceGraph.build (Graph.readonly g) program node_to_lrg in
            [%log.debug "Interference Graph:\n%a" InterferenceGraph.pp ifg];
            let* coloring = InterferenceGraph.color ifg (Graph.readonly g) in
            Ok coloring
        in
        if round > 10 then
          failwith "This should've finished by now"
        else
          let program = List.filter program ~f:(Graph.mem g) in
          match attempt g program with
          | Ok (reg_assoc, max_stack_slot) ->
              [%log.debug "Register allocation done in %d rounds" round];
              (program, reg_assoc, max_stack_slot)
          | Error { failed_ranges; self_conflicts } ->
              (* TODO: perhaps sort failed ranges to make sure we are deterministic *)
              let new_program =
                  Hashtbl.fold self_conflicts ~init:program
                    ~f:(fun ~key:lrg ~data:self_conflicting_nodes program ->
                      split_self_conflict g program lrg self_conflicting_nodes)
              in
              let new_program =
                  List.fold failed_ranges ~init:new_program ~f:(fun program lrg ->
                      split g program lrg)
              in
              loop new_program (round + 1)
    in
    let program, lrg_to_reg, max_stack_slot = loop program 1 in
    let reg_assoc = Hashtbl.create (module Machine_node.Any) in
    Hashtbl.iteri lrg_to_reg ~f:(fun ~key:range ~data:reg ->
        Set.iter range.nodes ~f:(fun n -> Hashtbl.set reg_assoc ~key:n ~data:reg));
    let program = post_alloc_cleanup g program reg_assoc in
    let alloc_res = { max_stack_slot; reg_assoc } in
    (program, alloc_res)
