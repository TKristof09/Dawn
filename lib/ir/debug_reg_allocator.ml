open Core

let allocate (g : (Machine_node.t, Graph.readwrite) Graph.t) (program : Machine_node.t list) =
    let reg_assign = Hashtbl.create (module Machine_node) in
    let processed = Hash_set.create (module Machine_node) in

    let try_free_stack_slot free_slots node =
        match Hashtbl.find reg_assign node with
        | Some (Registers.Stack _slot) ->
            let all_dependants_processed =
                Graph.get_dependants g node |> List.for_all ~f:(Hash_set.mem processed)
            in
            if all_dependants_processed then (* slot :: free_slots *)
              free_slots
            else
              free_slots
        | _ -> free_slots
    in

    let create_reload_mov (input_node : Machine_node.t) cfg out_reg =
        let mov_node : Machine_node.t =
            {
              Machine_node.id = Machine_node.next_id ();
              Machine_node.kind = Mov;
              Machine_node.ir_node = input_node.ir_node;
            }
        in
        Graph.add_dependencies g mov_node [ cfg; Some input_node ];
        Hashtbl.set reg_assign ~key:mov_node ~data:out_reg;
        mov_node
    in

    let create_spill_mov (node : Machine_node.t) cfg stack_slot =
        let mov_node : Machine_node.t =
            {
              Machine_node.id = Machine_node.next_id ();
              Machine_node.kind = Mov;
              Machine_node.ir_node = node.ir_node;
            }
        in
        let dependants = Graph.get_dependants g node in

        Graph.add_dependencies g mov_node [ cfg; Some node ];
        Hashtbl.set reg_assign ~key:mov_node ~data:(Registers.Stack stack_slot);

        (* Update all dependents to use the spill mov instead of the node *)
        List.iter dependants ~f:(fun use ->
            let dep_idx, _ =
                List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                    match dep with
                    | None -> false
                    | Some d -> Machine_node.equal d node)
            in
            Graph.set_dependency g use (Some mov_node) dep_idx);
        mov_node
    in

    let choose_register mask used_regs =
        let available =
            List.fold used_regs ~init:mask ~f:(fun mask reg -> Registers.Mask.remove mask reg)
        in
        match Registers.Mask.choose available with
        | Some (Registers.Reg r) -> Registers.Reg r
        | Some (Registers.Stack _) -> assert false
        | None -> failwith "No available register"
    in

    let is_data (n : Machine_node.t) i =
        match n.ir_node.typ with
        | Types.Memory
        | Control ->
            false
        | Tuple (Value l) -> (
            match List.nth_exn l i with
            | Types.Memory
            | Control ->
                false
            | _ -> true)
        | _ -> true
    in

    let rec transform_program acc next_stack_slot free_slots = function
        | [] -> List.rev acc
        | node :: rest when Machine_node.is_cheap_to_clone node ->
            transform_program acc next_stack_slot free_slots rest
        | node :: rest ->
            if Hash_set.mem processed node then
              assert false
            else if Poly.equal node.kind (Ideal Phi) then (
              Hash_set.add processed node;
              let cfg = Graph.get_dependency g node 0 in

              let dependencies = Graph.get_dependencies g node in
              let data_inputs =
                  List.filter_mapi dependencies ~f:(fun i n ->
                      match n with
                      | None -> None
                      | Some n -> if is_data n i then Some (n, i) else None)
              in

              let cloned, _ =
                  List.fold data_inputs ~init:([], []) ~f:(fun (cloned, used_regs) (n, i) ->
                      if Machine_node.is_cheap_to_clone n then (
                        let clone = { n with id = Machine_node.next_id () } in
                        Graph.add_dependencies g clone (Graph.get_dependencies g n);
                        let in_mask = Machine_node.get_out_reg_mask g clone 0 |> Option.value_exn in
                        let reg = choose_register in_mask used_regs in
                        Hashtbl.set reg_assign ~key:clone ~data:reg;
                        ((clone, i) :: cloned, reg :: used_regs))
                      else
                        (cloned, used_regs))
              in

              let spills, next_stack_slot, free_slots =
                  List.fold cloned ~init:([], next_stack_slot, free_slots)
                    ~f:(fun (spills, next_stack_slot, free_slots) (clone, dep_idx) ->
                      let phi_slot =
                          List.find_map data_inputs ~f:(fun (n, _) ->
                              match Hashtbl.find reg_assign n with
                              | None -> None
                              | Some (Reg _) -> None
                              | Some (Stack slot) -> Some (Registers.Stack slot))
                      in
                      let stack_slot, free_slots, did_allocate =
                          match phi_slot with
                          | Some (Registers.Stack slot) -> (slot, free_slots, false)
                          | Some (Registers.Reg _) -> assert false
                          | None -> (
                              match free_slots with
                              | slot :: rest -> (slot, rest, false)
                              | [] ->
                                  let slot = next_stack_slot in
                                  (slot, free_slots, true))
                      in

                      let spill_mov : Machine_node.t =
                          {
                            Machine_node.id = Machine_node.next_id ();
                            Machine_node.kind = Mov;
                            Machine_node.ir_node = clone.ir_node;
                          }
                      in
                      Graph.add_dependencies g spill_mov [ cfg; Some clone ];
                      Graph.set_dependency g node (Some spill_mov) dep_idx;
                      Hashtbl.set reg_assign ~key:spill_mov ~data:(Stack stack_slot);
                      Hash_set.add processed spill_mov;

                      let next_stack_slot =
                          if did_allocate then stack_slot + 1 else next_stack_slot
                      in
                      (spill_mov :: spills, next_stack_slot, free_slots))
              in
              let slot =
                  Graph.get_dependency g node 2 |> Option.value_exn |> Hashtbl.find reg_assign
              in
              if node.id = 57 then
                Printf.printf "SLOT: %s  %s\n"
                  ([%derive.show: Registers.loc option] slot)
                  ([%derive.show: Machine_node.t option list] (Graph.get_dependencies g node));
              (match slot with
              | None -> ()
              | Some slot -> Hashtbl.set reg_assign ~key:node ~data:slot);
              let cloned = List.map cloned ~f:fst in
              transform_program (spills @ (node :: cloned) @ acc) next_stack_slot free_slots rest)
            else (
              Hash_set.add processed node;

              let cfg = Graph.get_dependency g node 0 in

              let dependencies = Graph.get_dependencies g node in
              let data_inputs =
                  List.filter_mapi dependencies ~f:(fun i n ->
                      match n with
                      | None -> None
                      | Some n -> if is_data n i then Some (n, i) else None)
              in

              let clonable n =
                  if Machine_node.is_cheap_to_clone n then
                    Some n
                  else
                    match
                      n.kind
                    with
                    | Mov ->
                        let n = Graph.get_dependency g n 1 |> Option.value_exn in
                        if Machine_node.is_cheap_to_clone n then
                          Some n
                        else
                          None
                    | _ -> None
              in
              let reload_movs, _ =
                  List.fold data_inputs ~init:([], []) ~f:(fun (movs, used_regs) (in_node, idx) ->
                      match clonable in_node with
                      | Some clonable ->
                          let clone = { clonable with id = Machine_node.next_id () } in
                          Graph.add_dependencies g clone (Graph.get_dependencies g clonable);
                          let in_mask =
                              Machine_node.get_in_reg_mask g node (idx - 1) |> Option.value_exn
                          in
                          let reg = choose_register in_mask used_regs in
                          Hashtbl.set reg_assign ~key:clone ~data:reg;
                          (clone :: movs, reg :: used_regs)
                      | None -> (
                          assert (Hash_set.mem processed in_node);
                          match Hashtbl.find reg_assign in_node with
                          | Some (Registers.Stack _) ->
                              let in_mask =
                                  Machine_node.get_in_reg_mask g node (idx - 1) |> Option.value_exn
                              in
                              let reg = choose_register in_mask used_regs in
                              let reload_mov = create_reload_mov in_node cfg reg in
                              Hashtbl.set reg_assign ~key:reload_mov ~data:reg;
                              (reload_mov :: movs, reg :: used_regs)
                          | Some (Reg Flags) -> (movs, used_regs)
                          | x ->
                              failwithf "Invalid reg assign found %s for %s -- %s"
                                ([%derive.show: Registers.loc option] x)
                                (Machine_node.show in_node) (Machine_node.show node) ()))
              in
              let reload_movs = List.rev reload_movs in

              (* no reload movs can happen for things like if/set node that only use flags *)
              if List.is_empty reload_movs then
                ()
              else
                List.iteri data_inputs ~f:(fun i (input_node, _) ->
                    let reload_mov = List.nth_exn reload_movs i in
                    let dep_idx, _ =
                        List.findi_exn dependencies ~f:(fun _ dep ->
                            match dep with
                            | None -> false
                            | Some d -> Machine_node.equal d input_node)
                    in
                    Graph.set_dependency g node (Some reload_mov) dep_idx;
                    Hash_set.add processed reload_mov);

              let new_segment, next_stack_slot, free_slots =
                  match Machine_node.get_out_reg_mask g node 0 with
                  | Some mask -> (
                      match Registers.Mask.choose mask with
                      | Some (Reg Flags) ->
                          Hashtbl.set reg_assign ~key:node ~data:(Reg Flags);
                          (node :: reload_movs, next_stack_slot, free_slots)
                      | Some reg ->
                          Hashtbl.set reg_assign ~key:node ~data:reg;

                          (* put all phi inputs into same stack slot. If any has already been assigned use that slot otherwise get a new slot *)
                          let phi_slot =
                              match
                                Graph.get_dependants g node
                                |> List.find ~f:(fun n -> Poly.equal n.kind (Ideal Phi))
                              with
                              | None -> None
                              | Some phi ->
                                  let deps = Graph.get_dependencies g phi in
                                  List.find_map deps
                                    ~f:
                                      (Option.bind ~f:(fun n ->
                                           match Hashtbl.find reg_assign n with
                                           | None -> None
                                           | Some (Reg _) -> None
                                           | Some (Stack slot) -> Some (Registers.Stack slot)))
                          in
                          let stack_slot, free_slots, did_allocate =
                              match phi_slot with
                              | Some (Registers.Stack slot) -> (slot, free_slots, false)
                              | Some (Registers.Reg _) -> assert false
                              | None -> (
                                  match free_slots with
                                  | slot :: rest -> (slot, rest, false)
                                  | [] ->
                                      let slot = next_stack_slot in
                                      (slot, free_slots, true))
                          in

                          let spill_mov = create_spill_mov node cfg stack_slot in
                          Hash_set.add processed spill_mov;

                          let new_segment = spill_mov :: node :: reload_movs in

                          let next_stack_slot =
                              if did_allocate then stack_slot + 1 else next_stack_slot
                          in
                          (new_segment, next_stack_slot, free_slots)
                      | None -> assert false)
                  | None -> (node :: reload_movs, next_stack_slot, free_slots)
              in
              let free_slots =
                  (* Try to free stack slots for inputs that are no longer needed *)
                  List.fold data_inputs ~init:free_slots ~f:(fun free_slots (n, _) ->
                      try_free_stack_slot free_slots n)
              in
              transform_program (new_segment @ acc) next_stack_slot free_slots rest)
    in

    let new_program = transform_program [] 0 [] (List.tl_exn program) in
    (new_program, reg_assign)
