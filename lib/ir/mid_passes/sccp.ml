open Core

(* This SCCP pass is used both for the usual constant propagation but also for
   dead code detection and also for type propagation which is later used for
   type checking. These can all be done in the same pass as our type system
   represents control/dead control and also constants. 

   NOTE: we don't actually eliminate dead control nor do we replace nodes with
   constants yet so this pass is kind of half finished for now.
 *)

module NodeSet = struct
  include Set.Make_plain (struct
    include Node.Any
    include Comparator.Make (Node.Any)
  end)
end

type extra_deps_tbl = (Node.any, NodeSet.t) Hashtbl.t
type min_int_types_tbl = (Node.any, Types.t) Hashtbl.t

let as_binop : type a b. (a, b) Node.t -> (Node.binop, Node.data) Node.t =
   fun n ->
    match n.kind with
    | Data Add -> n
    | Data Sub -> n
    | Data Div -> n
    | Data Mul -> n
    | Data Eq -> n
    | Data NEq -> n
    | Data Lt -> n
    | Data LEq -> n
    | Data Gt -> n
    | Data GEq -> n
    | Data BAnd -> n
    | Data BOr -> n
    | Data Lsh -> n
    | Data Rsh -> n
    | _ -> assert false

let set_type g n new_type =
    (* We can't assert monotonicity because for integers we track a minimum
       type (= max bit width) which raises monotonically during sccp. The node
       type then gets clamped to this min type. So node type can actually raise
       in these cases. But since this min type moves monotonically it will terminate
       at some point and from then on the node type drops monotonically so the whole
       SCCP terminates *)
    n.Node.typ <- new_type;
    Node.G.get_dependants g n

let backwards_prop_min_integer_type (min_integer_types : (Node.any, Types.t) Base.Hashtbl.t) g
    (Node.AnyNode n) min_type =
    assert (Types.is_a min_type (Integer All));
    let update min_type n =
        let changed = ref false in
        Hashtbl.update min_integer_types n ~f:(function
          | None ->
              changed := true;
              min_type
          | Some t ->
              let t_new = Types.join t min_type in
              changed := not (Types.equal t t_new);
              t_new);
        !changed
    in

    match n.Node.kind with
    | Data Add
    | Data Sub
    | Data Mul
    | Data Div
    | Data Phi
    | Data BAnd
    | Data BOr
    | Data (Param _) ->
        ignore (update min_type (AnyNode n));
        Node.G.get_dependencies_list g n
        |> List.filter_opt
        |> List.filter ~f:(fun (Node.AnyNode n) -> Option.is_none n.min_typ)
        |> List.filter ~f:(update min_type)
    | Data Lsh
    | Data Rsh ->
        ignore (update min_type (AnyNode n));
        let n = as_binop n in
        let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let changed1 = update min_type (AnyNode lhs) in
        (* TODO: rhs of shift needs to be smaller (e.g. if lhs is i32 then rhs has to be u5) *)
        let changed2 = update min_type (AnyNode rhs) in
        let l = if changed1 then [ Node.AnyNode lhs ] else [] in
        if changed2 then AnyNode rhs :: l else l
    | _ -> []

let work : type a b.
    Linker.t ->
    extra_deps_tbl ->
    min_int_types_tbl ->
    Node.G.readwrite Node.G.t ->
    (a, b) Node.t ->
    type_fn:
      (Node.G.readonly Node.G.t -> (a, b) Node.t -> (new_type:Types.t * extra_deps:Node.any list)) ->
    Node.any list =
   fun linker extra_node_deps min_integer_types g n ~type_fn ->
    let ~new_type, ~extra_deps = type_fn (Node.G.readonly g) n in

    let integer_min_type_changes =
        match Hashtbl.find min_integer_types (AnyNode n) with
        | Some min_type -> backwards_prop_min_integer_type min_integer_types g (AnyNode n) min_type
        | None -> (
            match n.Node.min_typ with
            | Some (Integer _ as min_type) ->
                backwards_prop_min_integer_type min_integer_types g (AnyNode n) min_type
            | _ -> [])
    in

    List.iter extra_deps ~f:(fun d ->
        Hashtbl.update extra_node_deps d ~f:(function
          | None -> NodeSet.singleton (AnyNode n)
          | Some s -> Set.add s (AnyNode n)));

    let new_type =
        match new_type with
        | Types.Integer (Value new_i) -> (
            match Hashtbl.find min_integer_types (AnyNode n) with
            | None -> new_type
            | Some (Integer (Value i) as min_type) ->
                (* If the type fits in the min_type's range then leave it alone
                   otherwise use min_type. This is to constrain integers to
                   some width because otherwise when a phi node at a loop head
                   fully widens then uses of that phi can go outside the range.
                   E.g. in (i is i32) while(i <= 10) { i+= 1} the phi fully
                   widens to i32 range but the add still executes so it's range
                   becomes [i32_min + 1; i32_max + 1] which is bad. *)
                if Z.leq i.min new_i.min && Z.leq new_i.max i.max then
                  Types.make_int ~num_widens:new_i.num_widens ?fixed_width:i.fixed_width new_i.min
                    new_i.max
                else
                  min_type
            | Some (Integer All) ->
                (* this is full i64 range *)
                Types.make_int ~num_widens:new_i.num_widens ~fixed_width:64 new_i.min new_i.max
            | Some _ -> assert false)
        | Integer All -> (
            (* try to constrain Integer All to some fixed width if we know the width we want *)
            match Hashtbl.find min_integer_types (AnyNode n) with
            | None -> new_type
            | Some min_type -> min_type)
        | _ -> new_type
    in
    let new_work =
        if Types.equal new_type n.typ then
          []
        else
          let new_work = set_type g n new_type in
          let new_fun_links =
              match new_type with
              | FunPtr _ ->
                  Node.G.get_dependants g n
                  |> List.filter_map
                       ~f:(fun (AnyNode dep) : (Node.fun_call, Node.ctrl) Node.t option ->
                         match dep.kind with
                         | Ctrl FunctionCall ->
                             let (AnyData fun_ptr) = Fun_node.get_call_fun_ptr g dep in
                             if Node.equal fun_ptr n then
                               Some dep
                             else
                               None
                         | _ -> None)
                  |> List.fold ~init:[] ~f:(fun acc dep ->
                      let fun_nodes = Linker.link linker g dep in
                      List.fold fun_nodes ~init:acc ~f:(fun acc fun_node ->
                          let params =
                              Node.G.get_dependants g fun_node
                              |> List.filter_map ~f:(fun (AnyNode n) ->
                                  match n.kind with
                                  | Data (Param _) -> Some (Node.AnyNode n)
                                  | _ -> None)
                          in
                          (Node.AnyNode fun_node :: params) @ acc))
              | _ -> []
          in
          let extras =
              Hashtbl.find extra_node_deps (AnyNode n) |> Option.value ~default:NodeSet.empty
          in
          new_fun_links @ Set.to_list extras @ new_work
    in
    integer_min_type_changes @ new_work

let do_data_node : type a.
    Linker.t ->
    extra_deps_tbl ->
    min_int_types_tbl ->
    Node.G.readwrite Node.G.t ->
    (a, Node.data) Node.t ->
    Node.any list =
   fun linker extra_node_deps min_integer_types g n ->
    match n.kind with
    | Data Constant -> (* constant stays the same as it was *) []
    | Data Add
    | Data Sub
    | Data Mul
    | Data Div ->
        let n = as_binop n in
        work linker extra_node_deps min_integer_types g n ~type_fn:Arithmetic_nodes.compute_type
    | Data Lsh
    | Data Rsh
    | Data BAnd
    | Data BOr ->
        let n = as_binop n in
        work linker extra_node_deps min_integer_types g n ~type_fn:Bitop_nodes.compute_type
    | Data (Proj _) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Proj_node.compute_type
    | Data Eq
    | Data NEq
    | Data Lt
    | Data LEq
    | Data Gt
    | Data GEq ->
        let n = as_binop n in
        work linker extra_node_deps min_integer_types g n ~type_fn:Bool_nodes.compute_type
    | Data Phi -> work linker extra_node_deps min_integer_types g n ~type_fn:Phi_node.compute_type
    | Data (Param _) ->
        (* params are just fancy phi nodes so this should work the same *)
        work linker extra_node_deps min_integer_types g n ~type_fn:Phi_node.compute_type
    | Data (External _) -> (* this is just like a constant *) []
    | Data Cast -> failwith "TODO"
    | Data (Load field) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let { Node.mem; ptr } : Node.load = Node.G.get_dependencies_exn g n in
            let (AnyData ptr) = Option.value_exn ptr in
            match ptr.typ with
            | Ptr p -> (~new_type:p, ~extra_deps:[])
            | ANY -> (~new_type:ANY, ~extra_deps:[])
            | ALL -> (~new_type:ALL, ~extra_deps:[])
            | _ -> assert false)
    | Data AddrOf ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let { Node.place; offset } = Node.G.get_dependencies_exn g n in
            let (AnyData place) = Option.value_exn place in
            (~new_type:(Ptr place.typ), ~extra_deps:[]))
    | Data (AddrOfField field) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let { Node.place; offset } = Node.G.get_dependencies_exn g n in
            let (AnyData place) = Option.value_exn place in
            match place.typ with
            | Struct _
            | Trait _ ->
                let t = Types.get_field_type place.typ ~include_trait_impl:true field in
                if Option.is_none t then
                  (~new_type:ALL, ~extra_deps:[])
                else
                  let t = Option.value_exn t in
                  if offset |> Option.is_some && Types.is_a t (Array All) then
                    let t = Types.get_array_element_type t in
                    (~new_type:(Ptr t), ~extra_deps:[])
                  else
                    (~new_type:(Ptr t), ~extra_deps:[])
            | ANY -> (~new_type:(Ptr ANY), ~extra_deps:[])
            | _ -> (~new_type:(Ptr ALL), ~extra_deps:[]))
    | Data Deref ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let { Node.mem; ptr } : Node.deref = Node.G.get_dependencies_exn g n in
            let (AnyData ptr) = Option.value_exn ptr in
            let t =
                match ptr.typ with
                | Ptr p -> p
                | ANY -> ANY
                | _ -> ALL
            in
            (~new_type:t, ~extra_deps:[]))
    | ForwardRef _ ->
        (* handled in do_node function *)
        assert false

let do_ctrl_node : type a.
    Linker.t ->
    extra_deps_tbl ->
    min_int_types_tbl ->
    Node.G.readwrite Node.G.t ->
    (a, Node.ctrl) Node.t ->
    Node.any list =
   fun linker extra_node_deps min_integer_types g n ->
    match n.kind with
    | Ctrl Start ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:(Types.Tuple (Value [ Control; Memory ])), ~extra_deps:[]))
    | Ctrl Stop ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:Types.Control, ~extra_deps:[]))
    | Ctrl (Proj _) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Proj_node.compute_type
    | Ctrl If -> work linker extra_node_deps min_integer_types g n ~type_fn:If_node.compute_type
    | Ctrl Region ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Region_node.compute_type
    | Ctrl Loop ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let { Node.entry; backedge } = Node.G.get_dependencies_exn g n in
            let (AnyCtrl entry) = Option.value_exn entry in
            (~new_type:entry.typ, ~extra_deps:[]))
    | Ctrl (Function _) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Fun_node.compute_fun_node_type
    | Ctrl Return ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let { Node.mem; data } = Node.G.get_dependencies_exn g n in
            let (AnyMem mem) = Option.value_exn mem in
            let (AnyData data) = Option.value_exn data in
            let (AnyNode ctrl) = Node.G.get_ctrl_exn g n in
            let new_type = Types.Tuple (Value [ ctrl.typ; mem.typ; data.typ ]) in
            (~new_type, ~extra_deps:[]))
    | Ctrl FunctionCall ->
        let old_type = n.typ in
        let new_type =
            let (AnyNode ctrl) = Node.G.get_ctrl_exn g n in
            ctrl.typ
        in
        let { Node.fun_ptr; mem = _; args = _ } = Node.G.get_dependencies_exn g n in
        let (AnyData fun_ptr) = Option.value_exn fun_ptr in
        (* link calls to function when it just became reachable *)
        let param_work =
            if
              (not (Types.equal old_type Control))
              && Types.equal new_type Control
              && (not (Types.equal fun_ptr.typ ANY))
              && Types.is_a fun_ptr.typ (FunPtr All)
            then
              let fun_nodes = Linker.link linker g n in
              List.fold fun_nodes ~init:[] ~f:(fun acc fun_node ->
                  let params =
                      Node.G.get_dependants g fun_node
                      |> List.filter_map ~f:(fun (AnyNode n) ->
                          match n.kind with
                          | Data (Param _) -> Some (Node.AnyNode n)
                          | _ -> None)
                  in
                  (Node.AnyNode fun_node :: params) @ acc)
            else
              []
        in
        param_work
        @ work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type, ~extra_deps:[]))
    | Ctrl FunctionCallEnd ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Fun_node.compute_call_end_type

let do_mem_node : type a.
    Linker.t ->
    extra_deps_tbl ->
    min_int_types_tbl ->
    Node.G.readwrite Node.G.t ->
    (a, Node.mem) Node.t ->
    Node.any list =
   fun linker extra_node_deps min_integer_types g n ->
    match n.kind with
    | Mem (Store _) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:Memory, ~extra_deps:[]))
    | Mem New -> []
    | Mem Copy ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:Memory, ~extra_deps:[]))
    | Mem Phi -> work linker extra_node_deps min_integer_types g n ~type_fn:Phi_node.compute_type
    | Mem Param ->
        (* params are just fancy phi nodes so this should work the same *)
        work linker extra_node_deps min_integer_types g n ~type_fn:Phi_node.compute_type
    | Mem (Proj _) ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Proj_node.compute_type

let do_node : type a b.
    extra_deps_tbl ->
    min_int_types_tbl ->
    Node.G.readwrite Node.G.t ->
    Linker.t ->
    (a, b) Node.t ->
    Node.any list =
   fun extra_node_deps min_integer_types g linker n ->
    match n.kind with
    | Data _ -> do_data_node linker extra_node_deps min_integer_types g n
    | Ctrl _ -> do_ctrl_node linker extra_node_deps min_integer_types g n
    | Scope _ -> []
    | Mem _ -> do_mem_node linker extra_node_deps min_integer_types g n
    | ForwardRef _ ->
        (* ignore these, they will produce an error in type checking *)
        []

let run g linker =
    let worklist = Queue.create ~capacity:(Node.G.get_num_nodes g) () in
    Node.G.iter g ~f:(fun (Node.AnyNode n) ->
        (* TODO: I really dont like this type of skipping some kinds, looks like a mess waiting to happen *)
        (match n.kind with
        | Data Constant
        | Data (External _) ->
            (* constants keep their original type since these are fixed and dont need any type inference *)
            ()
        | Mem New ->
            (* new needs to keep the type it will produce *)
            ()
        | _ -> n.typ <- ANY);
        Queue.enqueue worklist (Node.AnyNode n));

    let fun_nodes =
        Node.G.fold g ~init:[]
          ~f:(fun
              (acc : (Node.fun_def, Node.ctrl) Node.t list)
              (AnyNode n)
              :
              (Node.fun_def, Node.ctrl) Node.t list
            ->
            match n.kind with
            | Ctrl (Function _) -> n :: acc
            | _ -> acc)
    in
    (* unlink start from function nodes as we only want to care about functions if someone actually callls them, the start->function node connection was just for convenience *)
    List.iter fun_nodes ~f:(fun n -> Node.G.unlink_ctrl g n);

    let extra_node_deps = Hashtbl.create (module Node.Any) in
    let min_integer_types = Hashtbl.create (module Node.Any) in
    let rec loop i =
        match Queue.dequeue worklist with
        | None -> i
        | Some (AnyNode n) ->
            let news = do_node extra_node_deps min_integer_types g linker n in
            Queue.enqueue_all worklist news;
            loop (i + 1)
    in
    let num_iters = loop 0 in
    [%log.debug "SCCP took %d iters" num_iters];
    (* relink function nodes to start because we'll unlink them from FunctionCalls during scheduling so if they arent linked to start they'd be unreachable i think *)
    List.iter fun_nodes ~f:(fun n ->
        let (AnyNode start) = Node.G.get_start g in
        Node.G.set_ctrl g n start)
