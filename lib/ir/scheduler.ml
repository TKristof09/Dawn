open Core

let rev_post_order_cfg g node =
    let visited = Hash_set.create (module Machine_node.Any) in
    Hash_set.add visited node;

    let rec dfs (Machine_node.AnyNode node) acc =
        if Machine_node.is_control_node node then (
          Hash_set.add visited (AnyNode node);

          let new_acc =
              (* Use fold right to get the true branch of if statements first, this helps put the loop body before the loop exit *)
              Machine_node.G.get_dependants g node
              |> List.filter ~f:(fun (AnyNode n) -> Machine_node.is_control_node n)
              |> List.fold_right
                   ~f:(fun dep current_acc ->
                     if not (Hash_set.mem visited dep) then
                       dfs dep current_acc
                     else
                       current_acc)
                   ~init:acc
          in

          Machine_node.AnyNode node :: new_acc)
        else
          acc
    in
    dfs node []

(* consider caching this *)
let rec idepth g (Machine_node.AnyNode n) =
    let ctrl = Machine_node.G.get_ctrl g n in
    match n.kind with
    | Ideal Start
    | FunctionProlog _ ->
        0
    | Ideal Region ->
        let { Machine_node.ctrl_inputs } = Machine_node.G.get_dependencies_exn g n in
        ctrl_inputs
        |> List.fold ~init:0 ~f:(fun acc n ->
            match n with
            | Some n -> max acc (idepth g n)
            | None -> acc)
    | Ideal Loop ->
        let { Machine_node.entry; backedge = _ } = Machine_node.G.get_dependencies_exn g n in
        1 + idepth g entry
    | _ when Machine_node.is_control_node n -> 1 + idepth g (Option.value_exn ctrl)
    | _ -> idepth g (Option.value_exn ctrl)

(* consider caching this *)
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

let rec dom g (Machine_node.AnyNode n) =
    match n.kind with
    | Ideal Start
    | FunctionProlog _ ->
        assert false
    | Ideal Loop ->
        let { Machine_node.entry; backedge = _ } = Machine_node.G.get_dependencies_exn g n in
        entry
    | Ideal Region ->
        let { Machine_node.ctrl_inputs } = Machine_node.G.get_dependencies_exn g n in
        ctrl_inputs |> List.filter_opt |> List.reduce_exn ~f:(common_dom g)
    | _ -> Machine_node.G.get_ctrl_exn g n

and common_dom g lhs rhs =
    let (AnyNode lhs_n) = lhs in
    let (AnyNode rhs_n) = rhs in
    if Machine_node.equal lhs_n rhs_n then
      lhs
    else
      let comp = idepth g lhs - idepth g rhs in
      if comp >= 0 then
        common_dom g (dom g lhs) rhs
      else
        common_dom g lhs (dom g rhs)

let get_function : type a b. (a, b) Machine_node.t -> int =
   fun n ->
    let (AnyNode ir_node) = n.ir_node in
    match ir_node.parent_fun with
    | None -> 0
    | Some idx -> idx

let schedule_early g =
    let already_scheduled =
        Hash_set.create ~size:(Machine_node.G.get_num_nodes g) (module Machine_node.Any)
    in
    let start =
        Machine_node.G.find g ~f:(fun (AnyNode n) ->
            match n.kind with
            | FunctionProlog _ -> true
            | _ -> false)
        |> Option.value ~default:(Machine_node.G.get_start g)
    in
    let rec schedule (Machine_node.AnyNode node) =
        assert (not (Machine_node.is_control_node node));
        if not (Hash_set.mem already_scheduled (AnyNode node)) then (
          Hash_set.add already_scheduled (AnyNode node);
          let deps = Machine_node.G.get_dependencies_list g node in
          List.filter_map deps ~f:(function
            | None -> None
            | Some (AnyNode n) -> (
                match n.kind with
                | Ideal Phi
                | Param _
                | _
                  when Machine_node.is_control_node n ->
                    None
                | _ -> Some (Machine_node.AnyNode n)))
          |> List.iter ~f:schedule;
          let is_pinned =
              match node.kind with
              | Ideal Phi
              | DProj _ ->
                  true
              | _ -> false
          in
          if not is_pinned then
            let AnyNode scheduled_n, _ =
                List.fold
                  (List.tl deps |> Option.value ~default:[])
                  ~init:(start, idepth g start)
                  ~f:(fun (max_n, max_depth) n ->
                    match n with
                    | None -> (max_n, max_depth)
                    | Some (AnyNode n) ->
                        let cfg = Machine_node.G.get_ctrl_exn g n in
                        let d = idepth g cfg in
                        if d > max_depth then
                          (cfg, d)
                        else
                          (max_n, max_depth))
            in
            Machine_node.G.set_ctrl g node scheduled_n)
    in
    rev_post_order_cfg g start
    |> List.iter ~f:(fun (AnyNode n) ->
        assert (Machine_node.is_control_node n);
        Machine_node.G.get_dependencies_list g n
        |> List.filter_opt
        |> List.iter ~f:(fun (AnyNode n) ->
            match n.kind with
            | _ when Machine_node.is_control_node n -> ()
            | _ -> schedule (AnyNode n));
        match n.kind with
        | Ideal Region
        | Ideal Loop ->
            Machine_node.G.get_dependants g n
            |> List.filter ~f:(fun (AnyNode n) ->
                match n.kind with
                | Ideal Phi -> true
                | _ -> false)
            |> List.iter ~f:schedule
        | _ -> ())

let schedule_late g =
    let m = Hashtbl.create ~size:(Machine_node.G.get_num_nodes g) (module Machine_node.Any) in
    let is_forward_edge : type a b c d. (a, b) Machine_node.t -> (c, d) Machine_node.t -> bool =
       fun node dependant ->
        match dependant.kind with
        | Ideal Loop ->
            let { Machine_node.entry = _; backedge = AnyNode backedge } =
                Machine_node.G.get_dependencies_exn g dependant
            in
            not (Machine_node.equal node backedge)
        | Ideal Phi -> (
            let (AnyNode ctrl) = Machine_node.G.get_ctrl_exn g dependant in
            match ctrl.kind with
            | Ideal Loop ->
                let (AnyNode backedge) =
                    Machine_node.get_phi_backedge (Machine_node.G.readonly g) dependant
                    |> Option.value_exn
                in
                not (Machine_node.equal node backedge)
            | _ -> true)
        | _ -> true
    in
    let get_block (Machine_node.AnyNode node) (Machine_node.AnyNode dependant) =
        match dependant.kind with
        | Ideal Phi ->
            let { Machine_node.phi_inputs } = Machine_node.G.get_dependencies_exn g dependant in
            let (AnyNode region) = Machine_node.G.get_ctrl_exn g dependant in
            let region = Machine_node.unpack_exn region (Ideal Region) in
            let { Machine_node.ctrl_inputs } = Machine_node.G.get_dependencies_exn g region in
            List.zip_exn ctrl_inputs phi_inputs
            |> List.find_map_exn ~f:(fun (ctrl, data) ->
                match data with
                | None -> None
                | Some (AnyNode data) ->
                    if Machine_node.equal data node then Some (Option.value_exn ctrl) else None)
        | _ -> Hashtbl.find_exn m (AnyNode dependant)
    in

    let find_best (Machine_node.AnyNode early) late =
        let rec get_path (Machine_node.AnyNode cur) =
            if Machine_node.equal cur early then
              [ Machine_node.AnyNode early ]
            else
              AnyNode cur :: get_path (dom g (AnyNode cur))
        in
        let is_better best cur =
            loop_depth g cur < loop_depth g best
            || idepth g cur > idepth g best
            ||
            let (AnyNode best) = best in
            match best.kind with
            | Jmp _ -> true
            | _ -> false
        in
        let best =
            List.reduce_exn (get_path late) ~f:(fun best n -> if is_better best n then n else best)
        in
        assert (
          let (AnyNode best) = best in
          Machine_node.is_blockhead best);
        best
    in
    let rec schedule node =
        if not (Hashtbl.mem m node) then (
          let (AnyNode node_unwrapped) = node in
          (match node_unwrapped.kind with
          | Ideal Phi
          | Param _
          | CalleeSave _ ->
              Hashtbl.set m ~key:node ~data:(Machine_node.G.get_ctrl_exn g node_unwrapped)
          | DProj _ ->
              let (AnyNode cfg) = Machine_node.G.get_ctrl_exn g node_unwrapped in
              if Machine_node.is_control_node cfg then
                Hashtbl.set m ~key:node ~data:(AnyNode cfg)
              else
                ()
          | _ when Machine_node.is_control_node node_unwrapped ->
              if Machine_node.is_blockhead node_unwrapped then
                Hashtbl.set m ~key:node ~data:node
              else
                Hashtbl.set m ~key:node ~data:(Machine_node.G.get_ctrl_exn g node_unwrapped)
          | _ -> ());

          List.iter (Machine_node.G.get_dependants g node_unwrapped) ~f:(fun (AnyNode n) ->
              if is_forward_edge node_unwrapped n then schedule (AnyNode n));

          if not (Hashtbl.mem m node) then
            let lca =
                List.map (Machine_node.G.get_dependants g node_unwrapped) ~f:(get_block node)
                |> List.reduce_exn ~f:(common_dom g)
            in
            let (AnyNode early) = Machine_node.G.get_ctrl_exn g node_unwrapped in
            let early =
                if Machine_node.is_control_node early then
                  Machine_node.AnyNode early
                else
                  Machine_node.G.get_ctrl_exn g early
            in
            let best = find_best early lca in
            Hashtbl.set m ~key:node ~data:best)
    in
    schedule (Machine_node.G.get_start g);
    Hashtbl.iteri m ~f:(fun ~key ~data ->
        let (AnyNode key) = key in
        let (AnyNode data) = data in
        match key.kind with
        | Ideal Phi -> ()
        | _ when Machine_node.is_control_node key -> ()
        | DProj _ -> ()
        | _ -> Machine_node.G.set_ctrl g key data)

let score g (Machine_node.AnyNode n) =
    match n.kind with
    | DProj _
    | Ideal (CProj 0) ->
        1049
    | Ideal (CProj 1) -> 1050
    | Ideal Phi -> 1030
    | Param i -> 1030 - i (* make them be in order so it looks nicer :) *)
    | CalleeSave r ->
        (* make them be in order so it looks nicer :) *)
        let idx =
            Registers.Mask.callee_save
            |> Registers.Mask.to_list
            |> List.findi_exn ~f:(fun _ r' -> Poly.equal (Registers.Reg r) r')
            |> fst
        in
        999 - idx
        (* just after the params *)
    | _ when Machine_node.is_control_node n -> 1
    | Ideal _ -> 500
    | _ ->
        (* this makes things like cmp nodes get low prio so they get put at the end of the block right before the jump so the flags don't get overwritten inbetween*)
        if
          Machine_node.G.get_dependants g n
          |> List.exists ~f:(fun (AnyNode n) -> Machine_node.is_control_node n)
        then
          10
        else
          500

let schedule_flat g =
    let schedule_main nodes =
        let scheduled = Dynarray.create () in
        let not_ready = Hash_set.of_list (module Machine_node.Any) nodes in
        List.iter nodes ~f:(fun (AnyNode n) ->
            if Machine_node.is_multi_output n then
              Machine_node.G.get_dependants g n
              |> List.iter ~f:(fun (AnyNode n') ->
                  match n'.kind with
                  | DProj _ -> Hash_set.add not_ready (AnyNode n')
                  | _ -> ()));
        let ready =
            Hash_set.filter not_ready ~f:(fun (AnyNode n) ->
                match n.kind with
                | Param _ -> true
                | _ ->
                    let deps = Machine_node.G.get_dependencies_list g n |> List.filter_opt in
                    List.for_all deps ~f:(fun x -> not (Hash_set.mem not_ready x)))
        in
        let not_ready = Hash_set.diff not_ready ready in
        let is_ready (Machine_node.AnyNode node) =
            List.for_all (Machine_node.G.get_dependencies_list g node) ~f:(function
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
          let (AnyNode best) = best in
          List.iter (Machine_node.G.get_dependants g best) ~f:(fun n ->
              if Hash_set.mem not_ready n && is_ready n then (
                Hash_set.add ready n;
                Hash_set.remove not_ready n))
        done;
        Dynarray.to_list scheduled
    in
    let cfg = rev_post_order_cfg g (Machine_node.G.get_start g) in
    cfg
    |> List.filter ~f:(fun (AnyNode n) -> Machine_node.is_blockhead n)
    |> List.map ~f:(fun (AnyNode bb) ->
        Machine_node.AnyNode bb
        :: (Machine_node.G.get_dependants g bb
           |> List.filter ~f:(fun (AnyNode n) -> not (Machine_node.is_blockhead n))
           |> schedule_main))

let duplicate_constants g function_graphs =
    let (AnyNode start) = Machine_node.G.get_start g in
    let consts =
        Machine_node.G.get_dependants g start
        |> List.filter_map ~f:(fun (AnyNode n) : (unit, unit) Machine_node.t option ->
            match n.kind with
            | Int _ -> Some (Machine_node.fix_tag n)
            | Ptr -> Some (Machine_node.fix_tag n)
            | Noop -> Some (Machine_node.fix_tag n)
            | _ -> None)
    in
    let module ConstNode = struct
      type t = (unit, unit) Machine_node.t

      let equal = Machine_node.equal
      let compare = Machine_node.compare
      let hash = Machine_node.hash
      let sexp_of_t = Machine_node.sexp_of_t (fun () -> Sexp.Atom "") (fun () -> Sexp.Atom "")
    end in
    let new_copies = Hashtbl.create (module Int) in
    List.iter consts ~f:(fun n ->
        Machine_node.G.get_dependants g n
        |> List.filter ~f:(fun (AnyNode use) ->
            match use.kind with
            | Param _ -> false
            | _ -> true)
        |> List.iter ~f:(fun (AnyNode use) ->
            let fun_idx = get_function use in
            if fun_idx <> 0 then
              let dep_idx, _ =
                  List.findi_exn (Machine_node.G.get_dependencies_list g use) ~f:(fun _ dep ->
                      match dep with
                      | None -> false
                      | Some (AnyNode dep) -> Machine_node.equal dep n)
              in
              let tbl =
                  Hashtbl.find_or_add new_copies fun_idx ~default:(fun _ ->
                      Hashtbl.create (module ConstNode))
              in
              Hashtbl.add_multi tbl ~key:n ~data:(Machine_node.AnyNode use, dep_idx)));
    List.iter function_graphs ~f:(fun g ->
        let (AnyNode fun_start) = Machine_node.G.get_start g in
        let fun_idx =
            match fun_start.kind with
            | FunctionProlog i -> i
            | Ideal Start -> 0
            | _ -> assert false
        in
        match Hashtbl.find new_copies fun_idx with
        | None -> ()
        | Some tbl ->
            Hashtbl.iteri tbl ~f:(fun ~key:old ~data ->
                let new_copy = { old with id = Machine_node.next_id () } in
                List.iter data ~f:(fun (AnyNode use, dep_idx) ->
                    Machine_node.G.replace_input g ~node:use ~from:old ~to_:new_copy);
                Machine_node.G.add_node g new_copy ();
                Machine_node.G.set_ctrl g new_copy fun_start))

let schedule g =
    let g = Machine_node.convert_graph g in
    let per_function_graphs =
        Machine_node.G.partition g
          ~f:(fun (AnyNode n) -> get_function n)
          ~get_start:(fun (AnyNode n) ->
            match n.kind with
            | FunctionProlog _ -> true
            | Ideal Start -> true
            | _ -> false)
          ~get_stop:(fun (AnyNode n) ->
            match n.kind with
            | Return -> true
            | Ideal Stop -> true
            | _ -> false)
    in
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
    let (AnyNode top_level_start) = Machine_node.G.get_start top_level in
    Machine_node.G.get_dependants top_level top_level_start
    |> List.iter ~f:(fun (AnyNode n) ->
        if Machine_node.G.get_dependants top_level n |> List.is_empty then
          Machine_node.G.remove_node top_level n);

    List.iter per_function_graphs ~f:(fun g ->
        (* unlink all calls. This means removing the
           FunctionProlog<--FunctionCall depedency and also the Param <-- arg
           depedencies *)
        let fun_prolog =
            Machine_node.G.find_map g
              ~f:(fun (AnyNode n) : (Machine_node.merge_point, unit) Machine_node.t option ->
                match n.kind with
                | FunctionProlog _ -> Some (Machine_node.fix_tag n)
                | _ -> None)
        in
        (match fun_prolog with
        | None -> ()
        | Some fun_prolog ->
            let { Machine_node.ctrl_inputs } = Machine_node.G.get_dependencies_exn g fun_prolog in
            List.iter ctrl_inputs ~f:(function
              | None -> ()
              | Some (AnyNode n) -> (
                  match n.kind with
                  | FunctionCall _ ->
                      let { Machine_node.fun_ptr = _; mem = _; args } =
                          Machine_node.G.get_dependencies_exn g n
                      in
                      List.iter args ~f:(fun (AnyNode arg) ->
                          let param =
                              Machine_node.G.get_dependants g arg
                              |> List.find_map_exn
                                   ~f:(fun
                                       (AnyNode n)
                                       :
                                       (Machine_node.phi, unit) Machine_node.t option
                                     ->
                                     match n.kind with
                                     | Param _ -> Some (Machine_node.fix_tag n)
                                     | _ -> None)
                          in
                          Graph.remove_dependency g ~node:param ~dep:arg);
                      Graph.remove_dependency g ~node:fun_prolog ~dep:n
                  | _ -> ())));

        let ret_node =
            Machine_node.G.find_map g
              ~f:(fun (AnyNode n) : (Machine_node.return, unit) Machine_node.t option ->
                match n.kind with
                | Return -> Some (Machine_node.fix_tag n)
                | _ -> None)
        in
        match ret_node with
        | None -> ()
        | Some ret_node ->
            (* unlink call ends *)
            let call_ends =
                Machine_node.G.get_dependants g ret_node
                |> List.filter_map
                     ~f:(fun
                         (AnyNode n) : (Machine_node.fun_call_end, unit) Machine_node.t option ->
                       match n.kind with
                       | FunctionCallEnd -> Some (Machine_node.fix_tag n)
                       | _ -> None)
            in
            (* We completely unlink all call end nodes. Doing it this way is
               easier than unlinking only the current ret_node from the
               call_end. And the end result will be the same since we go over
               every single ret_node in the program. (Only difference is that
               we might set a call_end's inputs to the empty list multiple
               times, but this is whatever) *)
            List.iter call_ends ~f:(fun n -> Machine_node.G.set_node_inputs g n { ret_nodes = [] });

            (* set up callee saved nodes *)
            (* if there is a return there must be a fun prolog (only top level
               has no function prolog but it also has stop node and not return
               node *)
            let fun_prolog = fun_prolog |> Option.value_exn in
            let callee_saves =
                List.map (Registers.Mask.callee_save |> Registers.Mask.to_list) ~f:(fun r ->
                    match r with
                    | Reg reg ->
                        let n = Machine_node.create_node (CalleeSave reg) fun_prolog.ir_node in
                        Machine_node.G.add_node g n ();
                        Machine_node.G.set_ctrl g n fun_prolog;
                        n
                    | _ -> assert false)
            in
            let ret_inputs = Machine_node.G.get_dependencies_exn g ret_node in
            Machine_node.G.set_node_inputs g ret_node { ret_inputs with callee_saves });
    (* FIXME schedule early pulls out nodes from branches of an if to before the if. They then get pulled back in to the branch in schedule_flat but it still feels wrong for schedule_early to be able to pull them out *)
    List.map per_function_graphs ~f:(fun g ->
        schedule_early g;
        schedule_late g;
        (g, schedule_flat g))
