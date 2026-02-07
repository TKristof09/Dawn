open Core

let id_counter = ref 0

let next_id () =
    incr id_counter;
    !id_counter

type cmp =
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq
[@@deriving show { with_path = false }, sexp_of]

type ideal =
    | Loop
    | CProj of int
    | Start
    | Stop
    | Region
    | Phi
[@@deriving show { with_path = false }, sexp_of]

type machine_node_kind =
    | Add
    | AddImm of int
    | Sub
    | SubImm of int
    | Mul
    | MulImm of int
    | Div
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm of int
    | RshImm of int
    | AndImm of int
    | OrImm of int
    | Cmp
    | CmpImm of int
    | Set of cmp
    | JmpAlways
    | Jmp of cmp
    | Int of int
    | Mov
    | DProj of int
    | FunctionProlog of int
    | Return
    | FunctionCall of int
    | FunctionCallEnd
    | Param of int
    | New
    | Store
    | Load
    (* nodes that have no machine equivalent *)
    | Ideal of ideal
[@@deriving show { with_path = false }, sexp_of]

type t = {
    id : int;
    mutable kind : machine_node_kind;
    ir_node : Node.t; [@opaque]
  }
[@@deriving show { with_path = false }, sexp_of]

let equal n n' = n.id = n'.id
let compare n n' = Int.compare n.id n'.id
let hash n = Int.hash n.id

let invert_cond cond =
    match cond with
    | Eq -> NEq
    | NEq -> Eq
    | Lt -> GEq
    | LEq -> Gt
    | Gt -> LEq
    | GEq -> Lt

let invert_jmp kind =
    match kind with
    | Jmp cond -> Jmp (invert_cond cond)
    | _ -> assert false

let is_cheap_to_clone n =
    match n.kind with
    | Int _ -> true
    | _ -> false

let is_control_node n =
    match n.kind with
    | Jmp _
    | JmpAlways
    | FunctionProlog _
    | FunctionCall _
    | FunctionCallEnd
    | Return
    | Ideal Start
    | Ideal Stop
    | Ideal Loop
    | Ideal Region
    | Ideal (CProj _) ->
        true
    | Ideal Phi
    | Add
    | AddImm _
    | Sub
    | SubImm _
    | Mul
    | MulImm _
    | Div
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm _
    | RshImm _
    | AndImm _
    | OrImm _
    | Cmp
    | CmpImm _
    | Set _
    | Int _
    | Mov
    | DProj _
    | Param _
    | New
    | Store
    | Load ->
        false

let is_blockhead n =
    match n.kind with
    | Ideal Start
    | Ideal (CProj _)
    | Ideal Region
    | Ideal Loop
    | Ideal Stop
    | FunctionProlog _
    | FunctionCallEnd ->
        true
    | _ -> false

let is_two_address n =
    match n.kind with
    | Add
    | AddImm _
    | Sub
    | SubImm _
    | Mul
    | MulImm _ ->
        true
    | _ -> false

let is_multi_output n =
    match n.ir_node.typ with
    | Tuple _ -> true
    | _ -> false

let get_in_reg_mask (_ : (t, 'a) Graph.t) (n : t) (i : int) =
    match n.kind with
    | Add
    | Sub
    | Mul ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.general_r)
    | AddImm _
    | SubImm _
    | MulImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Div ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.rax else Registers.Mask.div)
    | Lsh
    | Rsh ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.cl)
    | And
    | Or ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.general_r)
    | LshImm _
    | RshImm _
    | AndImm _
    | OrImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Int _ -> None
    | DProj _ -> None
    | Cmp ->
        assert (i <= 1);
        Some Registers.Mask.general_r
    | CmpImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_r
    | Set _ ->
        assert (i = 0);
        Some Registers.Mask.flags
    | JmpAlways -> None
    | Jmp _ ->
        assert (i = 0);
        Some Registers.Mask.flags
    | Mov ->
        assert (i = 0);
        Some Registers.Mask.all_and_stack
    | FunctionProlog _ -> None
    | Return ->
        assert (i = 0);
        Some Registers.Mask.rax
    | FunctionCall _ -> Some (Registers.Mask.x64_systemv i)
    | FunctionCallEnd -> None
    | Param _ -> None
    | New -> if i = 1 then Some (Registers.Mask.x64_systemv 0) else None
    | Store -> (
        match i with
        | 1 -> Some Registers.Mask.general_r (* base*)
        | 2 -> Some Registers.Mask.general_r (* index *)
        | 3 -> Some Registers.Mask.general_r (* value *)
        | _ -> failwithf "Invalid index %d for input reg mask of %s" i (show n) ())
    | Load -> (
        match i with
        | 1 -> Some Registers.Mask.general_r (* base*)
        | 2 -> Some Registers.Mask.general_r (* index *)
        | _ -> failwithf "Invalid index %d for input reg mask of %s" i (show n) ())
    | Ideal _ -> None

let rec get_out_reg_mask (g : (t, 'a) Graph.t) (n : t) (i : int) =
    match n.kind with
    | Add
    | Sub
    | Mul
    | AddImm _
    | SubImm _
    | MulImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Div ->
        assert (i = 0);
        Some Registers.Mask.rax
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm _
    | RshImm _
    | AndImm _
    | OrImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Int _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | DProj proj_i ->
        let in_node = Graph.get_dependencies g n |> List.hd_exn in
        Option.bind in_node ~f:(fun dep -> get_out_reg_mask g dep proj_i)
    | Cmp
    | CmpImm _ ->
        assert (i = 0);
        Some Registers.Mask.flags
    | Set _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Mov ->
        assert (i = 0);
        Some Registers.Mask.all_and_stack
    | JmpAlways -> None
    | Jmp _ -> None
    | FunctionProlog _ -> None
    | FunctionCall _ -> None
    | FunctionCallEnd ->
        if i = 1 then
          Some Registers.Mask.rax
        else
          None
    | Param idx ->
        assert (i = 0);
        Some (Registers.Mask.x64_systemv idx)
    | Return ->
        assert (i = 0);
        if i = 0 then Some Registers.Mask.rax else None
    | New -> if i = 1 then Some Registers.Mask.rax else None
    | Store -> None
    | Load -> Some Registers.Mask.general_w
    | Ideal _ -> None

let get_register_kills (n : t) =
    match n.kind with
    | FunctionCall _ -> Some Registers.Mask.caller_save
    | Div -> Some (Registers.Mask.of_list [ Reg RDX ])
    | New -> Some Registers.Mask.caller_save
    | _ -> None

let rec of_data_node g machine_g (kind : Node.data_kind) (n : Node.t) =
    let binop_commutative kind kind_imm =
        let deps = Graph.get_dependencies g n in
        match
          List.find_map deps ~f:(fun n ->
              Option.bind n ~f:(fun n ->
                  match n.typ with
                  | Integer (Value _) -> Some n
                  | _ -> None))
        with
        | None ->
            let node = { id = next_id (); kind; ir_node = n } in
            Graph.add_dependencies machine_g node [];
            Graph.add_dependencies machine_g node
              (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
            node
        | Some cn ->
            let value =
                match cn.typ with
                | Integer (Value v) -> v
                | _ -> assert false (* already checked in the List.find call above *)
            in
            let kind = kind_imm value in
            let node = { id = next_id (); kind; ir_node = n } in
            Graph.add_dependencies machine_g node [];
            let deps =
                List.filter deps ~f:(fun n ->
                    match n with
                    | None -> true
                    | Some n -> not (Node.equal cn n))
            in
            Graph.add_dependencies machine_g node
              (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
            node
    in
    let binop_non_commutative kind kind_imm =
        let deps = Graph.get_dependencies g n in
        match List.nth_exn deps 2 with
        | None -> assert false
        | Some dep -> (
            match dep.typ with
            | Integer (Value v) ->
                let kind = kind_imm v in
                let node = { id = next_id (); kind; ir_node = n } in
                Graph.add_dependencies machine_g node [];
                let deps =
                    List.filter deps ~f:(fun n ->
                        match n with
                        | None -> true
                        | Some n -> not (Node.equal dep n))
                in
                Graph.add_dependencies machine_g node
                  (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
                node
            | _ ->
                let node = { id = next_id (); kind; ir_node = n } in
                Graph.add_dependencies machine_g node [];
                Graph.add_dependencies machine_g node
                  (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
                node)
    in
    match kind with
    | Add -> binop_commutative Add (fun i -> AddImm i)
    | Sub -> binop_non_commutative Sub (fun i -> SubImm i)
    | Constant ->
        let kind =
            match n.typ with
            | Integer (Value i) -> Int i
            | _ -> assert false
        in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [ Some (Graph.get_start machine_g) ];
        node
    | Proj i ->
        let kind = DProj i in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq ->
        let cmp_deps = Graph.get_dependencies g n in
        let m_cmp =
            match kind with
            | Eq -> Eq
            | NEq -> NEq
            | Lt -> Lt
            | LEq -> LEq
            | Gt -> Gt
            | GEq -> GEq
            | _ -> assert false
        in

        let set_node =
            match
              List.find_mapi cmp_deps ~f:(fun i n ->
                  Option.bind n ~f:(fun n ->
                      match n.typ with
                      | Integer (Value _) -> Some (i, n)
                      | _ -> None))
            with
            | None ->
                let set_node = { id = next_id (); kind = Set m_cmp; ir_node = n } in
                let cmp_node = { id = next_id (); kind = Cmp; ir_node = n } in

                Graph.add_dependencies machine_g set_node [ Some cmp_node ];
                Graph.add_dependencies machine_g cmp_node
                  (List.map cmp_deps ~f:(Option.map ~f:(convert_node g machine_g)));
                set_node
            | Some (idx, cn) ->
                let value =
                    match cn.typ with
                    | Integer (Value v) -> v
                    | _ -> assert false (* already checked in the List.find call above *)
                in

                let m_cmp =
                    if idx = 2 then
                      m_cmp
                    else
                      match
                        m_cmp
                      with
                      | Eq
                      | NEq ->
                          m_cmp
                      | Lt -> Gt
                      | LEq -> GEq
                      | Gt -> Lt
                      | GEq -> LEq
                in
                let set_node = { id = next_id (); kind = Set m_cmp; ir_node = n } in
                let cmp_node = { id = next_id (); kind = CmpImm value; ir_node = n } in
                Graph.add_dependencies machine_g set_node [ Some cmp_node ];
                let deps =
                    List.filter cmp_deps ~f:(fun n ->
                        match n with
                        | None -> true
                        | Some n -> not (Node.equal cn n))
                in
                Graph.add_dependencies machine_g cmp_node
                  (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
                set_node
        in
        set_node
    | Phi ->
        let kind = Ideal Phi in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Mul -> binop_commutative Mul (fun i -> MulImm i)
    | Div ->
        let kind = Div in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        Graph.add_dependencies machine_g node
          (Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g)));
        node
    | Lsh -> binop_non_commutative Lsh (fun i -> LshImm i)
    | Rsh -> binop_non_commutative Rsh (fun i -> RshImm i)
    | BAnd -> binop_commutative And (fun i -> AndImm i)
    | BOr -> binop_commutative Or (fun i -> OrImm i)
    | Param i ->
        let kind = Param i in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        Graph.add_dependencies machine_g node
          (Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g)));
        node

and of_ctrl_node g machine_g (kind : Node.ctrl_kind) (n : Node.t) =
    let simple kind =
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    in
    match kind with
    | If ->
        let deps = Graph.get_dependencies g n in
        let op =
            List.filter_opt deps
            |> List.find_map_exn ~f:(fun n ->
                match n.kind with
                | Data Eq -> Some Eq
                | Data NEq -> Some NEq
                | Data Lt -> Some Lt
                | Data LEq -> Some LEq
                | Data Gt -> Some Gt
                | Data GEq -> Some GEq
                | _ -> None)
        in
        let kind = Jmp op in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps = deps |> List.map ~f:(Option.map ~f:(convert_node g machine_g)) in
        Graph.add_dependencies machine_g node deps;
        node
    | Stop ->
        let node = Graph.get_stop machine_g in
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Start ->
        let node = Graph.get_start machine_g in
        node
    | Proj i -> simple (Ideal (CProj i))
    | Loop -> simple (Ideal Loop)
    | Region -> simple (Ideal Region)
    | Function { ret = _; signature = _; idx } -> simple (FunctionProlog idx)
    | Return -> simple Return
    | FunctionCall ->
        let deps = Graph.get_dependencies g n in
        let fun_ptr = Fun_node.get_call_fun_ptr g n in
        let fun_idx = Types.get_fun_idx fun_ptr.typ in
        let kind = FunctionCall fun_idx in
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps =
            deps
            |> List.filter_map ~f:(function
              | None -> Some None
              | Some n ->
                  if Node.equal n fun_ptr then None else Some (Some (convert_node g machine_g n)))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | FunctionCallEnd -> simple FunctionCallEnd

and of_mem_node g machine_g kind (n : Node.t) =
    let simple kind =
        let node = { id = next_id (); kind; ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    in
    match kind with
    | Node.New -> simple New
    | Store -> (* TODO check for ops like add that can address memory directly *) simple Store
    | Load -> (* TODO check for ops like add that can address memory directly *) simple Load

and convert_node g machine_g (n : Node.t) =
    match
      Graph.find machine_g ~f:(fun mn ->
          match n.kind with
          | Ctrl Start
          | Ctrl Stop ->
              false
          | _ -> Node.equal mn.ir_node n)
    with
    | Some mn -> mn
    | None -> (
        match n.kind with
        | Data k -> of_data_node g machine_g k n
        | Ctrl k -> of_ctrl_node g machine_g k n
        | Mem k -> of_mem_node g machine_g k n
        | Scope _ -> assert false)

let find_dep machine_g n ~f =
    Graph.get_dependencies machine_g n
    |> List.findi ~f:(fun _ dep ->
        match dep with
        | None -> false
        | Some dep -> f dep)
    |> Option.map ~f:(fun (i, n) -> (i, Option.value_exn n))

let post_process (machine_g : (t, Graph.readwrite) Graph.t) =
    (* when changing a node's dependency we need to add a temp node that depends on the new_dep to make sure it doesn't get removed for not having any dependants. E.g. A jmp removes it's depedendancy on a set and set's it to the cmp directly. But the set might get removed if it has no dependants which in turn might remove the cmp for not having dependants *)
    let temp_node =
        { id = next_id (); kind = Int 0; ir_node = { typ = ANY; kind = Data Constant; id = 0 } }
    in
    Graph.fold machine_g ~init:[] ~f:(fun acc n ->
        match n.kind with
        | Jmp _ -> (
            let set_dep =
                find_dep machine_g n ~f:(fun dep ->
                    match dep.kind with
                    | Set _ -> true
                    | _ -> false)
            in
            let cmp_dep =
                Option.bind set_dep ~f:(fun (_, set) ->
                    find_dep machine_g set ~f:(fun dep ->
                        match dep.kind with
                        | Cmp
                        | CmpImm _ ->
                            true
                        | _ -> false))
            in
            match cmp_dep with
            | None -> acc
            | Some (_, cmp_n) ->
                let set_idx, set_n = Option.value_exn set_dep in
                let op =
                    match set_n.kind with
                    | Set op -> op
                    | _ -> assert false
                in
                n.kind <- Jmp op;
                (n, set_n, set_idx, cmp_n) :: acc)
        | _ -> acc)
    |> List.iter ~f:(fun (node, old_dep, idx, new_dep) ->
        Graph.add_dependencies machine_g temp_node [ Some new_dep ];
        Graph.remove_dependency machine_g ~node ~dep:old_dep;
        Graph.set_dependency machine_g node (Some new_dep) idx);
    (* it might be the case that there were no nodes to post process, in that
       case temp_node isn't added to the graph. This is fine *)
    try Graph.remove_node machine_g temp_node with
    | _ -> ()

module MachineGraphNode : Graph.GraphNode with type t = t = struct
  type nonrec t = t

  let show = show
  let pp = pp
  let equal = equal
  let semantic_equal n _ n' _ = equal n n'
  let hash = hash
  let compare = compare
  let sexp_of_t = sexp_of_t
  let is_persistent = Fun.const false
end

let convert_graph g =
    let start = { id = next_id (); kind = Ideal Start; ir_node = Graph.get_start g } in
    let stop = { id = next_id (); kind = Ideal Stop; ir_node = Graph.get_stop g } in

    let machine_g = Graph.create (module MachineGraphNode) start stop in
    convert_node g machine_g stop.ir_node |> ignore;
    post_process machine_g;
    machine_g
