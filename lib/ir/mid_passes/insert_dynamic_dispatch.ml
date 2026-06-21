open Core

(* For every function that takes in a Trait as parameter we swap out the struct
    given as argument with a vtable for the trait's functions. (At this stage
    type checking already ran so the struct is known to implement the needed
    functions. 
   TODO: the vtable is currently constructed at the call site and not just once in static memory.
   *)

let create_vtable g (Node.AnyMem mem) trait (Node.AnyData arg) call =
    let trait_fields =
        match trait with
        | Types.Trait (Value t) ->
            List.filter t.fields ~f:(fun (name, t) ->
                not (Types.equal t Void && String.count name ~f:(Char.equal '$') = 1))
        | _ -> assert false
    in
    let typ = trait in
    let vtable_size = Types.get_size typ in
    let size = Const_node.create_int g arg.loc ?parent_fun:arg.parent_fun vtable_size in
    let alloc =
        (* FIXME: this ctrl input is nonsense but i also should remove the ctrl input from New nodes *)
        let (AnyNode ctrl) = Node.G.get_ctrl_exn g call in
        let ctrl = Node.as_ctrl_exn ctrl in
        Mem_nodes.create_new g arg.loc ?parent_fun:arg.parent_fun ~ctrl ~mem ~size typ
    in
    let mem = Proj_node.create_mem ?parent_fun:arg.parent_fun g arg.loc alloc 0 in
    let vtable = Proj_node.create_data ?parent_fun:arg.parent_fun g arg.loc alloc 1 in
    let mem =
        List.fold trait_fields ~init:(Node.AnyMem mem) ~f:(fun (AnyMem mem) (field_name, field_t) ->
            let addr_field_vtable =
                Mem_nodes.create_addr_of_field g arg.loc ?parent_fun:arg.parent_fun vtable
                  field_name
            in
            let arg_fun_type = Types.get_field_type arg.typ field_name |> Option.value_exn in
            let fun_ptr =
                Const_node.create_from_type g arg.loc ?parent_fun:arg.parent_fun arg_fun_type
            in
            let store =
                Mem_nodes.create_store g arg.loc ?parent_fun:arg.parent_fun ~mem
                  ~ptr:addr_field_vtable field_name ~value:fun_ptr
            in
            AnyMem store)
    in
    (* TODO: somehow compute the type of vtable correctly as it might allow for
       some optimisations? Currently it is just the type of the trait in which
       each function ptr has multiple indices but this is an actual vtable so
       each fun ptr actually refercs to a single fun idx *)
    let vtable = Mem_nodes.create_addr_of g arg.loc ?parent_fun:arg.parent_fun vtable in
    (mem, Node.AnyNode vtable)

let do_node g fun_node =
    let { Node.call_sites } = Node.G.get_dependencies_exn g fun_node in
    let mem_param, data_params = Fun_node.get_param_nodes (Node.G.readonly g) fun_node in
    let mems =
        List.map call_sites ~f:(fun o -> Option.value_exn o)
        |> List.map ~f:(fun (AnyCtrl call_node) ->
            let call_node = Node.unpack_exn call_node (Ctrl FunctionCall) in
            let { Node.fun_ptr; mem; args } = Node.G.get_dependencies_exn g call_node in
            let old_mem = Option.value_exn mem in
            let (AnyMem mem) =
                List.zip_exn data_params args
                |> List.fold ~init:old_mem ~f:(fun mem (param, arg) ->
                    let (AnyData arg) = Option.value_exn arg in
                    match param.typ with
                    | Trait (Value t) ->
                        (* NOTE: this trait type param.typ might be less specific
                       than the actual parameter of the function as param.typ
                       is the meet of all inputs and it might be that all the
                       inputs have more in common than the single trait we
                       actually need. This still remains correct though but
                       vtable might end up bigger than absolutely needed *)
                        let mem, vtable = create_vtable g mem param.typ (AnyData arg) call_node in
                        Node.G.replace_input_unsafe g ~node:param ~from:(AnyNode arg) ~to_:vtable;
                        Node.G.replace_input_unsafe g ~node:call_node ~from:(AnyNode arg)
                          ~to_:vtable;
                        mem
                    | _ -> mem)
            in
            let (AnyMem old_mem) = old_mem in
            Node.G.replace_input_unsafe g ~node:call_node ~from:(AnyNode old_mem) ~to_:(AnyNode mem);
            Node.AnyMem mem)
    in
    (* The affected params are now going to be a Ptr to the vtable so recompute
       their types and replace their uses by a deref to keep downstream types
       correct. Also recalculate the param's type so downstream passes see the
       correct new type. *)
    List.iter data_params ~f:(fun p ->
        match p.typ with
        | Trait (Value _) ->
            let ~new_type, ~extra_deps = Phi_node.compute_type (Node.G.readonly g) p in
            p.typ <- new_type;
            let uses = Node.G.get_dependants g p in
            let deref = Mem_nodes.create_deref g p.loc ?parent_fun:p.parent_fun ~mem:mem_param p in
            List.iter uses ~f:(fun (AnyNode use) ->
                Node.G.replace_input_unsafe g ~node:use ~from:(AnyNode p) ~to_:(AnyNode deref))
        | _ -> ());

    Node.G.set_node_inputs g mem_param { phi_inputs = List.map mems ~f:Option.some }

let run g =
    let call_nodes =
        Node.G.fold g ~init:[]
          ~f:(fun
              (acc : (Node.fun_def, Node.ctrl) Node.t list)
              (AnyNode n)
              :
              (Node.fun_def, Node.ctrl) Node.t list
            ->
            match n.kind with
            | Ctrl (Function f) ->
                let param_types =
                    match f.signature with
                    | FunPtr (Value fptr) -> fptr.params
                    | _ -> assert false
                in
                if
                  List.exists param_types ~f:(fun t ->
                      (* we can't just use is_a t (Trait all) because is_a
                         struct (Trait all) is true since traits are right
                         below structs in the lattice *)
                      match t with
                      | Trait _ -> true
                      | _ -> false)
                then
                  n :: acc
                else
                  acc
            | _ -> acc)
    in
    List.iter call_nodes ~f:(do_node g)
