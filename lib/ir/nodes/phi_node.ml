open Core

let create_data g loc ?parent_fun ctrl nodes =
    let typ = List.map nodes ~f:(fun (Node2.AnyData n) -> n.typ) |> List.reduce_exn ~f:Types.meet in
    let n = Node2.create_data ?parent_fun loc typ Phi in
    Node2.G.add_node g n { Node2.phi_inputs = List.map nodes ~f:Option.some };
    Node2.G.set_ctrl g n ctrl;
    n

let create_mem g loc ?parent_fun ctrl nodes =
    let typ = List.map nodes ~f:(fun (Node2.AnyMem n) -> n.typ) |> List.reduce_exn ~f:Types.meet in
    let n = Node2.create_mem ?parent_fun loc typ Phi in
    Node2.G.add_node g n { Node2.phi_inputs = List.map nodes ~f:Option.some };
    Node2.G.set_ctrl g n ctrl;
    n

let create_data_no_backedge g loc ?parent_fun ctrl dep =
    let typ = dep.Node2.typ in
    let n = Node2.create_data ?parent_fun loc typ Phi in
    Node2.G.add_node g n { Node2.phi_inputs = [ None; Some (AnyData dep) ] };
    Node2.G.set_ctrl g n ctrl;
    n

let create_mem_no_backedge g loc ?parent_fun ctrl dep =
    let typ = dep.Node2.typ in
    let n = Node2.create_mem ?parent_fun loc typ Phi in
    Node2.G.add_node g n { Node2.phi_inputs = [ None; Some (AnyMem dep) ] };
    Node2.G.set_ctrl g n ctrl;
    n

let add_backedge_input : type a b c.
    Node2.G.readwrite Node2.G.t -> (a Node2.phi, b) Node2.t -> (c, b) Node2.t -> unit =
   fun g n dep ->
    (* keep the phi's original type as the type, it will get recomputed by the
       scope_node.merge_loop function after all phis have both their inputs *)
    let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
    match n.kind with
    | Data Phi ->
        Node2.G.set_node_inputs g n { Node2.phi_inputs = phi_inputs @ [ Some (Node2.AnyData dep) ] }
    | Data (Param _) -> failwith "Can't add backedge to param node"
    | Mem Phi ->
        Node2.G.set_node_inputs g n { Node2.phi_inputs = phi_inputs @ [ Some (Node2.AnyMem dep) ] }
    | Mem Param -> failwith "Can't add backedge to param node"
    | _ -> .

let add_input g phi inp =
    let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g phi in
    Node2.G.set_node_inputs g phi { phi_inputs = phi_inputs @ [ Some inp ] }


let compute_type : type a b.
    Node2.G.readonly Node2.G.t ->
    (a Node2.phi, b) Node2.t ->
    (new_type:Types.t * extra_deps:Node2.any list) =
   fun g n ->
    let (AnyNode region) = Node2.G.get_ctrl_exn g n in
    let ctrl_inputs =
        match region.kind with
        | Ctrl Region ->
            let { Node2.ctrl_inputs } = Node2.G.get_dependencies_exn g region in
            ctrl_inputs
        | Ctrl Loop ->
            let { Node2.entry; backedge } = Node2.G.get_dependencies_exn g region in
            [ entry; backedge ]
        | Ctrl (Function _) ->
            let { Node2.call_sites } = Node2.G.get_dependencies_exn g region in
            call_sites
        | _ -> assert false
    in
    let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
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
        | Some (Node2.AnyCtrl n) -> Some (Node2.AnyNode n)
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
