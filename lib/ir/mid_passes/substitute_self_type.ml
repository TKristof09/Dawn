open Core

(* This pass goes over every struct type that has implemented trait functions
   which have Self as parameter/return type and substitutes the Self with the
   actual struct type that implemented it. To this end it updates the types on
   the function's param nodes and also the struct's type itself. *)

let get_trait_funs_with_self = function
    | Types.Struct (Value { name; fields }) ->
        let funs =
            List.filter_map fields ~f:(fun (n, t) ->
                if String.is_prefix n ~prefix:"$" then
                  match
                    t
                  with
                  | FunPtr (Value { params; ret; fun_indices }) ->
                      if
                        Types.equal ret Self
                        || Types.equal ret (Ptr Self)
                        || List.exists params ~f:(fun t ->
                            Types.equal t Self || Types.equal t (Ptr Self))
                      then
                        Some t
                      else
                        None
                  | _ -> None
                else
                  None)
        in
        if List.is_empty funs then None else Some funs
    | _ -> None

let substitute_self_in_params g (Node.AnyNode n) fun_node =
    let _, params = Fun_node.get_param_nodes (Node.G.readonly g) fun_node in
    let base_type = n.typ in
    List.iter params ~f:(fun p ->
        if Types.equal p.typ Self then
          p.typ <- base_type
        else if Types.equal p.typ (Ptr Self) then
          p.typ <- Ptr base_type
        else
          ())

let substitute_in_fun_ptr (constant : ('a, 'b) Node.t) self_typ =
    match constant.typ with
    | FunPtr (Value { params; ret; fun_indices }) ->
        let fun_idx = Types.get_fun_idx constant.typ |> Option.value_exn in
        let new_params =
            List.map params ~f:(fun p ->
                match p with
                | Self -> self_typ
                | Ptr Self -> Types.Ptr self_typ
                | _ -> p)
        in
        let new_ret =
            match ret with
            | Self -> self_typ
            | Ptr Self -> Ptr self_typ
            | _ -> ret
        in
        constant.typ <- Types.make_fun_ptr ~idx:fun_idx new_params new_ret
    | _ -> ()

let run g linker =
    let types_with_trait_impls =
        Node.G.fold g ~init:[] ~f:(fun acc (AnyNode n) ->
            match n.kind with
            | _ when Types.is_a n.typ (Struct All) -> (
                match get_trait_funs_with_self n.typ with
                | None -> acc
                | Some funs -> (Node.AnyNode n, funs) :: acc)
            | _ -> acc)
    in
    (* We'll most likely process the same functions several times. This is fine imo as we just update the type of some param nodes so nothing computationally heavy.*)
    List.iter types_with_trait_impls ~f:(fun (AnyNode n, funs) ->
        [%log.debug "Substituting self in %a" Types.pp n.typ];
        (* This mutates the original n.typ which makes it so that all places
           that used that type also get updated without having to go and look
           for every place this type is used (which can be a lot) *)
        Types.substitute_self n.typ;
        let fun_indexes =
            List.fold funs ~init:Int.Set.empty ~f:(fun acc fun_typ ->
                let fun_idxs = ref [] in
                Linker.iter_fun_nodes linker fun_typ ~f:(fun fun_idx fun_node ->
                    fun_idxs := fun_idx :: !fun_idxs;
                    substitute_self_in_params g (AnyNode n) fun_node);
                List.fold !fun_idxs ~init:acc ~f:Set.add)
        in
        let (AnyNode start) = Node.G.get_start g in
        Node.G.get_dependants g start
        |> List.iter ~f:(fun (AnyNode c) ->
            match c.kind with
            | Data Constant when Types.is_a c.typ (FunPtr All) ->
                Types.get_fun_idx c.typ
                |> Option.iter ~f:(fun idx ->
                    if Set.mem fun_indexes idx then
                      substitute_in_fun_ptr c n.typ
                    else
                      ())
            | _ -> ()))
