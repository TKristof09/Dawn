open Core

(* NOTE: Whatever nodes we insert in this pass we need to make sure we
   correctly set the types (unless the node creation already sets it correctly)
   because the type propagation with sccp already happened and we probably
   don't want to rerun sccp just for a few nodes *)

type decomposition = { integer : int (* TODO floats, etc.. *) }

type descriptor = {
    field_name : string;
    eightbyte_idx : int;
    byte_start_inside_eightbyte : int;
    size : int;
  }

let get_param_idx n =
    match n.Node.kind with
    | Data (Param i) -> i
    | _ -> assert false

let decompose (t : Types.t) =
    (* TODO: calculate this better *)
    let fields =
        match t with
        | Struct (Value { name = _; fields }) -> fields
        | _ -> assert false
    in

    let _, max_eightbyte_idx =
        List.fold fields ~init:(0, 0) ~f:(fun (size_acc, res) (field_name, field_type) ->
            let size = Types.get_size field_type in
            let offset = size_acc % 8 in
            let idx =
                if offset + size > 8 then
                  (* didn't fit into eightbyte so have to put into next one *)
                  (size_acc / 8) + 1
                else
                  size_acc / 8
            in
            let size_acc = (idx * 8) + offset + size in
            (size_acc, idx))
    in
    if max_eightbyte_idx + 1 <= 2 then
      Some { integer = max_eightbyte_idx + 1 }
    else
      None

let get_descriptors fields =
    List.fold fields ~init:(0, []) ~f:(fun (size_acc, res) (field_name, field_type) ->
        let size = Types.get_size field_type in
        let offset = size_acc % 8 in
        let descriptor =
            if offset + size > 8 then (* didn't fit into eightbyte so have to put into next one *)
              {
                field_name;
                eightbyte_idx = (size_acc / 8) + 1;
                byte_start_inside_eightbyte = 0;
                size;
              }
            else
              {
                field_name;
                eightbyte_idx = size_acc / 8;
                byte_start_inside_eightbyte = offset;
                size;
              }
        in
        assert (descriptor.byte_start_inside_eightbyte + size <= 8);
        let size_acc =
            (descriptor.eightbyte_idx * 8) + descriptor.byte_start_inside_eightbyte + size
        in
        (size_acc, descriptor :: res))
    |> snd
    |> List.rev

let is_zero_ascending_range = function
    | [] -> false
    | lst ->
        let rec aux expected = function
            | [] -> true
            | x :: xs -> x = expected && aux (expected + 1) xs
        in
        aux 0 lst

let patch_up g ~in_mem ~fun_node (param : Node.t) decomp =
    let fields =
        match param.typ with
        | Struct (Value { name = _; fields }) -> fields
        | _ -> assert false
    in
    let param_idx = get_param_idx param in
    (* list of {field_name; eightbyte_idx; byte_start_inside_eightbyte } *)
    let descriptors = get_descriptors fields in
    let eightbytes =
        List.group descriptors ~break:(fun d d' -> d.eightbyte_idx <> d'.eightbyte_idx)
    in

    assert (
      List.map eightbytes ~f:(fun eb -> (List.hd_exn eb).eightbyte_idx) |> is_zero_ascending_range);

    let num_param_inputs = Graph.get_dependencies g param |> List.tl_exn |> List.length in
    let new_params =
        List.map eightbytes ~f:(fun eightbyte ->
            let eightbyte_idx = (List.hd_exn eightbyte).eightbyte_idx in
            (* TODO: once we support floats we need to handle float eightbytes here too *)
            let typ = Types.u64 in
            let new_param =
                Fun_node.create_param g param.loc fun_node typ (param_idx + eightbyte_idx)
            in
            Graph.add_dependencies g new_param (List.init num_param_inputs ~f:(Fun.const None));
            new_param)
    in
    List.iter eightbytes ~f:(fun descriptors ->
        let eightbyte_idx = (List.hd_exn descriptors).eightbyte_idx in
        let eightbyte = List.nth_exn new_params eightbyte_idx in
        Graph.get_dependencies g param
        |> List.tl_exn
        |> List.map ~f:(fun o -> Option.value_exn o)
        |> List.iteri ~f:(fun i arg ->
            let param_input_idx = i + 1 in
            let call_node = Graph.get_dependency g fun_node param_input_idx |> Option.value_exn in
            let mem =
                Graph.get_dependencies g call_node
                |> List.find_map_exn
                     ~f:
                       (Option.bind ~f:(fun n ->
                            match n.Node.typ with
                            | Memory -> Some n
                            | _ -> None))
            in
            let components =
                List.map descriptors ~f:(fun desc ->
                    let offset =
                        Types.get_offset arg.typ desc.field_name
                        |> Option.value_exn
                        |> Z.of_int
                        |> Types.make_int_const ~fixed_width:64
                        |> Const_node.create_from_type g param.loc
                    in
                    let field_val =
                        Mem_nodes.create_load g param.loc ~mem ~ptr:arg desc.field_name ~offset
                          arg.typ
                    in
                    field_val.typ <-
                      Types.get_field_type arg.typ desc.field_name |> Option.value_exn;

                    let input =
                        if desc.byte_start_inside_eightbyte <> 0 then (
                          let shift_amount =
                              Const_node.create_int g param.loc
                                (8 * desc.byte_start_inside_eightbyte)
                          in
                          let shifted = Bitop_nodes.create_lsh g param.loc field_val shift_amount in
                          let ~new_type, ~extra_deps:_ = Bitop_nodes.compute_type g shifted in
                          shifted.typ <- new_type;
                          shifted)
                        else
                          field_val
                    in
                    input)
            in
            let value =
                List.reduce_exn components ~f:(fun c1 c2 ->
                    let combined = Bitop_nodes.create_bor g param.loc c1 c2 in
                    let ~new_type, ~extra_deps:_ = Bitop_nodes.compute_type g combined in
                    combined.typ <- new_type;
                    combined)
            in
            Graph.set_dependency g eightbyte (Some value) param_input_idx;

            if eightbyte_idx = 0 then
              Graph.set_dependency g call_node (Some value) (param_idx + 2)
            else
              let call_node_deps = Graph.get_dependencies g call_node |> List.tl_exn in
              let rec aux idx l =
                  if idx = 0 then
                    Some value :: l
                  else
                    match
                      l
                    with
                    | [] -> None :: aux (idx - 1) []
                    | h :: t -> h :: aux (idx - 1) t
              in
              aux (param_idx + eightbyte_idx + 1) call_node_deps
              |> List.iteri ~f:(fun i dep ->
                  if i < List.length call_node_deps then
                    Graph.set_dependency g call_node dep (i + 1)
                  else
                    Graph.add_dependencies g call_node [ dep ])));

    List.iter new_params ~f:(fun p ->
        let ~new_type, ~extra_deps:_ = Phi_node.compute_type g p in
        p.typ <- new_type);

    (* HACK: we just reconstruct a struct in the function. This is not
               optimal, it would be better to use the incoming registers
               directly but this is much easier to implement and it's still correct *)
    let type_size = Types.get_size param.typ in
    let size =
        Const_node.create_from_type g param.loc
          (Types.make_int_const ~fixed_width:64 (Z.of_int type_size))
    in
    let typ = param.typ in
    let reconstructed = Mem_nodes.create_new g param.loc ~ctrl:fun_node ~mem:in_mem ~size typ in
    let mem = Proj_node.create g param.loc reconstructed 0 in
    let ptr = Proj_node.create g param.loc reconstructed 1 in
    let mem =
        List.fold descriptors ~init:mem ~f:(fun mem desc ->
            let eightbyte = List.nth_exn new_params desc.eightbyte_idx in
            let input =
                if desc.byte_start_inside_eightbyte <> 0 then (
                  let shift_amount =
                      Const_node.create_int g param.loc (8 * desc.byte_start_inside_eightbyte)
                  in
                  let shifted = Bitop_nodes.create_rsh g param.loc eightbyte shift_amount in
                  let ~new_type, ~extra_deps:_ = Bitop_nodes.compute_type g shifted in
                  shifted.typ <- new_type;
                  shifted)
                else
                  eightbyte
            in
            let value =
                if desc.size < 8 then (
                  let mask =
                      Const_node.create_zint g param.loc
                        (Z.sub (Z.shift_left Z.one (desc.size * 8)) Z.one)
                  in
                  let masked = Bitop_nodes.create_band g param.loc input mask in
                  let ~new_type, ~extra_deps:_ = Bitop_nodes.compute_type g masked in
                  masked.typ <- new_type;
                  masked)
                else
                  input
            in
            let offset =
                Types.get_offset typ desc.field_name
                |> Option.value_exn
                |> Z.of_int
                |> Types.make_int_const ~fixed_width:64
                |> Const_node.create_from_type g param.loc
            in
            Mem_nodes.create_store g param.loc ~mem ~ptr ~offset desc.field_name ~value)
    in

    Graph.get_dependants g param
    |> List.iter ~f:(fun use ->
        let dep_idx, _ =
            List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                match dep with
                | None -> false
                | Some dep -> Node.equal dep param)
        in
        Graph.set_dependency g use (Some ptr) dep_idx);

    Graph.remove_node g param;
    (mem, new_params)

let pass_by_ptr g p =
    (* Memory loads/stores work both on ptr and struct types since
                 structs are actually just pointers anyway. So we don't need to
                 patch up the body of the function to take into account that we
                 only pass in the address. This is purely a type system level
                 change *)
    Graph.get_dependencies g p
    |> List.tl_exn (* drop control input *)
    |> List.iteri ~f:(fun i arg ->
        match arg with
        | None -> ()
        | Some arg ->
            (* account for dropped control input *)
            let dep_idx = i + 1 in
            let ptr = Mem_nodes.create_addr_of g p.loc arg in
            Graph.set_dependency g p (Some ptr) dep_idx)

let do_node g n =
    match n.Node.kind with
    | Ctrl (Function _) ->
        let fun_node = n in
        let params =
            Graph.get_dependants g fun_node
            |> List.filter_map ~f:(fun n ->
                match n.kind with
                | Data (Param i) -> Some (i, n)
                | _ -> None)
            |> List.sort ~compare:(fun (i, _) (i', _) ->
                assert (i <> i');
                Int.compare i i')
        in
        let _, mem_param = List.hd_exn params in
        assert (Types.equal mem_param.typ Memory);
        let params = List.tl_exn params in

        let mem_param_orig_uses = Graph.get_dependants g mem_param in

        let max_regs_for_params = 6 in
        let _, new_params, mem, _ =
            List.fold params ~init:(0, [ mem_param ], mem_param, 0)
              ~f:(fun (used_up_regs, params_acc, mem, extra_param_offset) (i, p) ->
                p.kind <- Data (Param (i + extra_param_offset));

                if used_up_regs = max_regs_for_params then
                  (used_up_regs, p :: params_acc, mem, extra_param_offset)
                else
                  match
                    p.typ
                  with
                  | Integer _
                  | Bool _
                  | Ptr _
                  | FunPtr _ ->
                      (used_up_regs + 1, p :: params_acc, mem, extra_param_offset)
                  | Struct _ -> (
                      match decompose p.typ with
                      | None ->
                          pass_by_ptr g p;
                          (used_up_regs, p :: params_acc, mem, extra_param_offset)
                      | Some decomp ->
                          if decomp.integer + used_up_regs > max_regs_for_params then (
                            pass_by_ptr g p;
                            (used_up_regs, p :: params_acc, mem, extra_param_offset))
                          else
                            let mem, new_params = patch_up g ~in_mem:mem ~fun_node p decomp in
                            ( used_up_regs + decomp.integer,
                              List.rev_append new_params params_acc,
                              mem,
                              extra_param_offset + (List.length new_params - 1) ))
                  | _ -> failwith "idk how we got here")
        in
        let new_params = List.rev new_params in
        assert (List.map new_params ~f:get_param_idx |> is_zero_ascending_range);

        List.iter mem_param_orig_uses ~f:(fun use ->
            let dep_idx, _ =
                List.findi_exn (Graph.get_dependencies g use) ~f:(fun _ dep ->
                    match dep with
                    | None -> false
                    | Some dep -> Node.equal dep mem_param)
            in
            Graph.set_dependency g use (Some mem) dep_idx);

        (* HACK: This is a very hacky way of putting the dependants of the
           fun_node in the correct order. This is needed since other places
           assume that the param nodes are in order when retrieving via
           Graph.get_dependants g fun_node *)
        (*TODO: This should not be assumed by other places since our graph
           structure doesn't provide any assurances when it comes to the order
           of dependants, it only assures the order of dependencies. Or the
           graph structure should be changed.  *)
        List.iter new_params ~f:(fun param -> Graph.remove_dependency g ~node:param ~dep:fun_node);
        List.iter new_params ~f:(fun param -> Graph.set_dependency g param (Some fun_node) 0)
    | _ -> ()

let run g =
    let nodes = Graph.fold g ~init:[] ~f:(fun acc n -> n :: acc) in
    List.iter nodes ~f:(do_node g)
