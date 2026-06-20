open Core

let create_data g loc ?parent_fun ctrl nodes =
    let typ = List.map nodes ~f:(fun (Node.AnyData n) -> n.typ) |> List.reduce_exn ~f:Types.meet in
    let n = Node.create_data ?parent_fun loc typ Phi in
    Node.G.add_node g n { Node.phi_inputs = List.map nodes ~f:Option.some };
    Node.G.set_ctrl g n ctrl;
    n

let create_mem g loc ?parent_fun ctrl nodes =
    let typ = List.map nodes ~f:(fun (Node.AnyMem n) -> n.typ) |> List.reduce_exn ~f:Types.meet in
    let n = Node.create_mem ?parent_fun loc typ Phi in
    Node.G.add_node g n { Node.phi_inputs = List.map nodes ~f:Option.some };
    Node.G.set_ctrl g n ctrl;
    n

let create_data_no_backedge g loc ?parent_fun ctrl dep =
    let typ = dep.Node.typ in
    let n = Node.create_data ?parent_fun loc typ Phi in
    Node.G.add_node g n { Node.phi_inputs = [ Some (AnyData dep); None ] };
    Node.G.set_ctrl g n ctrl;
    n

let create_mem_no_backedge g loc ?parent_fun ctrl dep =
    let typ = dep.Node.typ in
    let n = Node.create_mem ?parent_fun loc typ Phi in
    Node.G.add_node g n { Node.phi_inputs = [ Some (AnyMem dep); None ] };
    Node.G.set_ctrl g n ctrl;
    n

let add_backedge_input : type a b c.
    Node.G.readwrite Node.G.t -> (a Node.phi, b) Node.t -> (c, b) Node.t -> unit =
   fun g n dep ->
    (* keep the phi's original type as the type, it will get recomputed by the
       scope_node.merge_loop function after all phis have both their inputs *)
    let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
    let entry =
        match phi_inputs with
        | [ (Some _ as entry); None ] -> entry
        | _ -> assert false
    in
    match n.kind with
    | Data Phi ->
        Node.G.set_node_inputs g n { Node.phi_inputs = [ entry; Some (Node.AnyData dep) ] }
    | Data (Param _) -> failwith "Can't add backedge to param node"
    | Mem Phi -> Node.G.set_node_inputs g n { Node.phi_inputs = [ entry; Some (Node.AnyMem dep) ] }
    | Mem Param -> failwith "Can't add backedge to param node"
    | _ -> .

let get_backedge_input g n =
    let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
    List.nth_exn phi_inputs 1

let get_entry_edge_input g n =
    let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
    List.nth_exn phi_inputs 0

let add_input g phi inp =
    let { Node.phi_inputs } = Node.G.get_dependencies_exn g phi in
    Node.G.set_node_inputs g phi { phi_inputs = phi_inputs @ [ Some inp ] }

let compute_type : type a b.
    Node.G.readonly Node.G.t ->
    (a Node.phi, b) Node.t ->
    (new_type:Types.t * extra_deps:Node.any list) =
   fun g n ->
    let (AnyNode region) = Node.G.get_ctrl_exn g n in
    let ctrl_inputs =
        match region.kind with
        | Ctrl Region ->
            let { Node.ctrl_inputs } = Node.G.get_dependencies_exn g region in
            ctrl_inputs
        | Ctrl Loop ->
            let { Node.entry; backedge } = Node.G.get_dependencies_exn g region in
            [ entry; backedge ]
        | Ctrl (Function _) ->
            let { Node.call_sites } = Node.G.get_dependencies_exn g region in
            call_sites
        | _ -> assert false
    in
    let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
    let new_type =
        List.zip_exn ctrl_inputs phi_inputs
        |> List.filter_map ~f:(fun (c, d) ->
            match (c, d) with
            | Some (AnyCtrl c), Some d -> (
                match c.typ with
                | DeadControl -> None
                | Control -> (
                    match n.kind with
                    | Data Phi ->
                        let (AnyData d) = d in
                        Some d.typ
                    | Data (Param _) ->
                        let (AnyData d) = d in
                        Some d.typ
                    | Mem Phi ->
                        let (AnyMem d) = d in
                        Some d.typ
                    | Mem Param ->
                        let (AnyMem d) = d in
                        Some d.typ
                    | _ -> .)
                | ANY -> None
                | _ -> None)
            | _ -> None)
        |> List.fold ~init:Types.ANY ~f:(fun t t' -> Types.meet t t')
    in
    (* TODO: join with the initial type so that the type stays in the original "domain" e.g. it stays int bottom instead of global bottom. this would probably help produce better errors *)

    let any_of_ctrl n =
        match n with
        | None -> None
        | Some (Node.AnyCtrl n) -> Some (Node.AnyNode n)
    in
    let old_type = n.typ in
    match (region.kind, new_type, old_type) with
    | ( Ctrl Loop,
        Integer (Value { min; max; num_widens }),
        Integer (Value { min = old_min; max = old_max; num_widens = old_num_widens }) )
    | ( Ctrl (Function _),
        Integer (Value { min; max; num_widens }),
        Integer (Value { min = old_min; max = old_max; num_widens = old_num_widens }) )
      when not (Types.equal old_type new_type) ->
        let new_type =
            if (not (Types.is_constant new_type)) && num_widens <= old_num_widens then
              (* TODO: we want to store the original type of the phi and only
                 widen up to that. So e.g. a u8 doesnt need to be widened down
                 so much, only to the full u8 range *)
              Types.widen_int new_type Types.i64
            else
              new_type
        in
        (~new_type, ~extra_deps:(ctrl_inputs |> List.filter_map ~f:any_of_ctrl))
    | _ -> (~new_type, ~extra_deps:(ctrl_inputs |> List.filter_map ~f:any_of_ctrl))
