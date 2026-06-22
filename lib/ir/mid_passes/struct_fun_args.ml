open Core

(* This pass is to transform function calls that involve passing in structs as
   arguments in order to conform to SystemV ABI calling convention. Structs can
   either get broken up into "eightbytes" and passed in using normal registers
   or they can be passed in by pointer if the struct is too big. 

   Inside the function we either reconstruct the struct object if passed in via
   eightbytes or we dereference it if passed in via pointer. This is to ensure
   that downstream passes still see the expected types.

   (NOTE: this reconstruction is rather expensive at the moment since it
   involves a New and several Stores but in the future once we implement some
   memory op optimisation passes that cost should probably mostly go away)
*)

(* SystemV ABI uses 6 integer registers for params *)
let max_regs_for_params = 6

(* NOTE: Whatever nodes we insert in this pass we need to make sure we
   correctly set the types (unless the node creation already sets it correctly)
   because the type propagation with sccp already happened and we probably
   don't want to rerun sccp just for a few nodes *)

type decomposition = { integer : int (* TODO floats, etc.. *) }

type descriptor = {
    field_name : string;
    field_type : Types.t;
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
    List.filter fields ~f:(fun (_, t) -> not (Types.equal t Void))
    |> List.fold ~init:(0, []) ~f:(fun (size_acc, res) (field_name, field_type) ->
        let size = Types.get_size field_type in
        let offset = size_acc % 8 in
        let descriptor =
            if offset + size > 8 then (* didn't fit into eightbyte so have to put into next one *)
              {
                field_name;
                field_type;
                eightbyte_idx = (size_acc / 8) + 1;
                byte_start_inside_eightbyte = 0;
                size;
              }
            else
              {
                field_name;
                field_type;
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

let patch_up g ~in_mem:(Node.AnyMem in_mem) ~fun_node param decomp =
    let fields =
        match param.Node.typ with
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

    let { Node.phi_inputs = param_inputs } = Node.G.get_dependencies_exn g param in
    let { Node.call_sites } = Node.G.get_dependencies_exn g fun_node in
    let eightbytes =
        List.map eightbytes ~f:(fun descriptors ->
            let eightbyte_idx = (List.hd_exn descriptors).eightbyte_idx in
            let typ = Types.u64 in
            let eightbyte =
                Fun_node.create_param ?parent_fun:param.parent_fun g param.loc fun_node typ
                  (param_idx + eightbyte_idx)
            in
            (descriptors, eightbyte))
    in
    List.iter eightbytes ~f:(fun (descriptors, eightbyte_param_node) ->
        let eightbyte_param_inputs =
            param_inputs
            |> List.zip_exn call_sites
            |> List.map ~f:(fun (co, ao) -> (Option.value_exn co, Option.value_exn ao))
            |> List.map ~f:(fun (AnyCtrl call_node, AnyData arg) ->
                let call_node = Node.unpack_exn call_node (Ctrl FunctionCall) in
                let parent_fun = call_node.parent_fun in
                let { Node.fun_ptr; mem; args = _ } = Node.G.get_dependencies_exn g call_node in
                let (AnyMem mem) = Option.value_exn mem in
                let components =
                    List.map descriptors ~f:(fun desc ->
                        let field_ptr =
                            Mem_nodes.create_addr_of_field ?parent_fun g arg.loc arg desc.field_name
                        in
                        let field_val =
                            Mem_nodes.create_load ?parent_fun g arg.loc ~mem ~ptr:field_ptr
                              desc.field_name arg.typ
                        in
                        field_val.typ <-
                          Types.get_field_type arg.typ desc.field_name |> Option.value_exn;

                        let input =
                            if desc.byte_start_inside_eightbyte <> 0 then (
                              let shift_amount =
                                  Const_node.create_int ?parent_fun g arg.loc
                                    (8 * desc.byte_start_inside_eightbyte)
                              in
                              let shifted =
                                  Bitop_nodes.create_lsh ?parent_fun g arg.loc field_val
                                    shift_amount
                              in
                              let ~new_type, ~extra_deps:_ =
                                  Bitop_nodes.compute_type (Node.G.readonly g) shifted
                              in
                              shifted.typ <- new_type;
                              Node.AnyData shifted)
                            else
                              AnyData field_val
                        in
                        input)
                in
                List.reduce_exn components ~f:(fun (AnyData c1) (AnyData c2) ->
                    let combined = Bitop_nodes.create_bor ?parent_fun g arg.loc c1 c2 in
                    let ~new_type, ~extra_deps:_ =
                        Bitop_nodes.compute_type (Node.G.readonly g) combined
                    in
                    combined.typ <- new_type;
                    AnyData combined))
        in

        (* set the inputs to the eightbyte's param node to the new values *)
        Node.G.set_node_inputs g eightbyte_param_node
          { Node.phi_inputs = List.map eightbyte_param_inputs ~f:Option.some };
        (* calculate the correct type for the eightbyte *)
        let ~new_type, ~extra_deps:_ =
            Phi_node.compute_type (Node.G.readonly g) eightbyte_param_node
        in
        eightbyte_param_node.typ <- new_type);

    (* insert the new values into the arg list of each call site. Replace
           the original arg by the first eightbyte and insert the additional
           eightbytes to right after it. *)
    let rec add_call_args call_args new_args start_idx =
        match call_args with
        | [] -> assert false
        | h :: t ->
            if start_idx = 0 then
              new_args @ t
            else
              h :: add_call_args t new_args (start_idx - 1)
    in
    let eightbytes_per_call_site =
        List.map eightbytes ~f:(fun (_, p) ->
            let { Node.phi_inputs } = Node.G.get_dependencies_exn g p in
            phi_inputs)
        (* 
           here we have [[arg1_at_call1; arg1_at_call2; ..]; [arg2_at_call1; arg2_at_call2]; ...]
           but we want [[arg1_call1; arg2_call1; ...]; [arg1_call2; arg2_call2; ...] ... ]
        *)
        |> List.transpose_exn
    in
    List.zip_exn call_sites eightbytes_per_call_site
    |> List.iter ~f:(fun (call_site, eightbytes) ->
        let (AnyCtrl call_node) = Option.value_exn call_site in
        let call_node = Node.unpack_exn call_node (Ctrl FunctionCall) in
        let { Node.fun_ptr; mem; args } = Node.G.get_dependencies_exn g call_node in
        Node.G.set_node_inputs g call_node
          { Node.fun_ptr; mem; args = add_call_args args eightbytes param_idx });

    (* HACK: we just reconstruct a struct in the function. This is not
               optimal, it would be better to use the incoming registers
               directly but this is much easier to implement and it's still correct *)
    let type_size = Types.get_size param.typ in
    let size =
        Const_node.create_from_type ?parent_fun:param.parent_fun g param.loc
          (Types.make_int_const ~fixed_width:64 (Z.of_int type_size))
    in
    let typ = param.typ in
    let reconstructed =
        Mem_nodes.create_new ?parent_fun:param.parent_fun g param.loc ~ctrl:fun_node ~mem:in_mem
          ~size typ
    in
    let mem = Proj_node.create_mem ?parent_fun:param.parent_fun g param.loc reconstructed 0 in
    let ptr = Proj_node.create_data ?parent_fun:param.parent_fun g param.loc reconstructed 1 in
    let mem =
        List.fold descriptors ~init:(Node.AnyMem mem) ~f:(fun (AnyMem mem) desc ->
            let _, eightbyte = List.nth_exn eightbytes desc.eightbyte_idx in
            let (AnyData input) =
                if desc.byte_start_inside_eightbyte <> 0 then (
                  let shift_amount =
                      Const_node.create_int ?parent_fun:param.parent_fun g param.loc
                        (8 * desc.byte_start_inside_eightbyte)
                  in
                  let shifted =
                      Bitop_nodes.create_rsh ?parent_fun:param.parent_fun g param.loc eightbyte
                        shift_amount
                  in
                  let ~new_type, ~extra_deps:_ =
                      Bitop_nodes.compute_type (Node.G.readonly g) shifted
                  in
                  shifted.typ <- new_type;
                  Node.AnyData shifted)
                else
                  AnyData eightbyte
            in
            let (AnyData value) =
                if desc.size < 8 then (
                  let mask =
                      Const_node.create_zint ?parent_fun:param.parent_fun g param.loc
                        (Z.sub (Z.shift_left Z.one (desc.size * 8)) Z.one)
                  in
                  let masked =
                      Bitop_nodes.create_band ?parent_fun:param.parent_fun g param.loc input mask
                  in
                  masked.typ <- desc.field_type;
                  Node.AnyData masked)
                else
                  AnyData input
            in
            let field_ptr =
                Mem_nodes.create_addr_of_field ?parent_fun:param.parent_fun g param.loc ptr
                  desc.field_name
            in
            Node.AnyMem
              (Mem_nodes.create_store ?parent_fun:param.parent_fun g param.loc ~mem ~ptr:field_ptr
                 desc.field_name ~value))
    in

    (* make all the users of the param use the newly allocated struct instead *)
    Node.G.get_dependants g param
    |> List.iter ~f:(fun (AnyNode use) ->
        Node.G.replace_input_unsafe g ~node:use ~from:(AnyNode param) ~to_:(AnyNode ptr));

    Node.G.remove_node g param;
    (mem, List.map eightbytes ~f:snd)

let pass_by_ptr g ~mem:(Node.AnyMem mem) p =
    (* Dereference the param afterwards so that users see the actual struct
        type rather than a pointer *)
    let { Node.phi_inputs } = Node.G.get_dependencies_exn g p in
    let new_inputs =
        List.map phi_inputs
          ~f:
            (Option.map ~f:(fun (Node.AnyData arg) ->
                 Node.AnyData (Mem_nodes.create_addr_of ?parent_fun:p.parent_fun g p.loc arg)))
    in
    Node.G.set_node_inputs g p { Node.phi_inputs = new_inputs };
    let param_users = Node.G.get_dependants g p in
    let deref = Mem_nodes.create_deref g p.loc ?parent_fun:p.parent_fun ~mem p in
    List.iter param_users ~f:(fun (AnyNode user) ->
        Node.G.replace_input_unsafe g ~node:user ~from:(AnyNode p) ~to_:(AnyNode deref))

let do_node g fun_node =
    let mem_param, params = Fun_node.get_param_nodes (Node.G.readonly g) fun_node in

    let mem_param_orig_uses = Node.G.get_dependants g mem_param in

    let _, new_params, mem, _ =
        List.foldi params ~init:(0, [], Node.AnyMem mem_param, 0)
          ~f:(fun i (used_up_regs, params_acc, mem, extra_param_offset) p ->
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
                      pass_by_ptr g ~mem p;
                      (used_up_regs + 1, p :: params_acc, mem, extra_param_offset)
                  | Some decomp ->
                      if decomp.integer + used_up_regs > max_regs_for_params then (
                        pass_by_ptr g ~mem p;
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

    let (AnyMem mem) = mem in
    List.iter mem_param_orig_uses ~f:(fun (AnyNode use) ->
        Node.G.replace_input_unsafe g ~node:use ~from:(AnyNode mem_param) ~to_:(AnyNode mem))

let run g =
    let nodes =
        Node.G.fold g ~init:[]
          ~f:(fun
              (acc : (Node.fun_def, Node.ctrl) Node.t list)
              (AnyNode n)
              :
              (Node.fun_def, Node.ctrl) Node.t list
            ->
            match n.kind with
            | Ctrl (Function f) ->
                (* TODO: dead code elim should happen before this pass in the future so this is a temporary fix until then *)
                if Types.equal n.typ Control then
                  n :: acc
                else
                  acc
            | _ -> acc)
    in
    List.iter nodes ~f:(do_node g)
