open Core

let id_counter = ref 0

let next_id () =
    incr id_counter;
    !id_counter

let reset_id () = id_counter := 0

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
    | External of string
[@@deriving show { with_path = false }, sexp_of]

module Z = struct
  include Z

  let sexp_of_t z = Sexp.Atom (Z.to_string z)

  let t_of_sexp = function
      | Sexp.Atom s -> Z.of_string s
      | _ -> failwith "Z.t_of_sexp: expected atom"

  let pp fmt z = Format.pp_print_string fmt (Z.to_string z)
end

type machine_node_kind =
    | Int of Z.t
    | Ptr
    | ZeroExtend
    | SignExtend
    | Add
    | AddImm of Z.t
    | Sub
    | SubImm of Z.t
    | Mul
    | MulImm of Z.t
    | Div
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm of Z.t
    | RshImm of Z.t
    | AndImm of Z.t
    | OrImm of Z.t
    | Cmp
    | CmpImm of Z.t
    | Set of cmp
    | JmpAlways
    | Jmp of cmp
    | Mov
    | DProj of int
    | FunctionProlog of int
    | Return
    | FunctionCall of int option
    | FunctionCallEnd
    | Param of int
    | CalleeSave of Registers.reg
    | New
    | Store
    | Load
    | Noop
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
    | Int _
    | Ptr
    | Cmp
    | CmpImm _ ->
        true
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
    | Ptr
    | Mov
    | DProj _
    | Param _
    | New
    | Store
    | Load
    | CalleeSave _
    | Ideal (External _)
    | Noop
    | ZeroExtend
    | SignExtend ->
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
    | MulImm _
    | Lsh
    | LshImm _
    | Rsh
    | RshImm _
    | And
    | AndImm _
    | Or
    | OrImm _ ->
        true
    | Int _
    | Ptr
    | Div
    | Cmp
    | CmpImm _
    | Set _
    | JmpAlways
    | Jmp _
    | Mov
    | ZeroExtend
    | SignExtend
    | DProj _
    | FunctionProlog _
    | Return
    | FunctionCall _
    | FunctionCallEnd
    | Param _
    | CalleeSave _
    | New
    | Store
    | Load
    | Ideal _
    | Noop ->
        false

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
    | Ptr -> None
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
        (* inputs: 0: memory; 1: value; 2+: callee saved regs *)
        let is_void =
            match n.ir_node.typ with
            | Tuple (Value [ _; _; t ]) -> Types.equal t Void
            | _ -> false
        in
        if i = 0 then
          None
        else if i = 1 then
          if is_void then
            None
          else
            Some Registers.Mask.rax (* retutrn value *)
        else
          let calle_saved = Registers.Mask.callee_save |> Registers.Mask.to_list in
          Some (Registers.Mask.of_list [ List.nth_exn calle_saved (i - 2) ])
    | FunctionCall (Some _) ->
        (* skip memory input, known function call so no fun_ptr as input which makes memory at index 0 *)
        if i = 0 then
          None
        else
          Some (Registers.Mask.x64_systemv (i - 1))
    | FunctionCall None ->
        (* When the call target is not compile time known the first input to
           the FunctionCall node is the function ptr then memory and arguments come after *)
        if i = 0 then
          Some Registers.Mask.general_w
        else if i = 1 then
          None
        else
          Some (Registers.Mask.x64_systemv (i - 2))
    | FunctionCallEnd -> None
    | Param _ -> None
    | CalleeSave _ -> None
    | New -> if i = 1 then Some (Registers.Mask.x64_systemv 0) else None
    | Store -> (
        match i with
        | 0 -> None
        | 1 -> Some Registers.Mask.general_r (* base*)
        | 2 -> Some Registers.Mask.general_r (* index *)
        | 3 -> Some Registers.Mask.general_r (* value *)
        | _ -> failwithf "Invalid index %d for input reg mask of %s" i (show n) ())
    | Load -> (
        match i with
        | 0 -> None
        | 1 -> Some Registers.Mask.general_r (* base*)
        | 2 -> Some Registers.Mask.general_r (* index *)
        | _ -> failwithf "Invalid index %d for input reg mask of %s" i (show n) ())
    | Noop -> None
    | ZeroExtend ->
        assert (i = 0);
        Some Registers.Mask.general_r
    | SignExtend ->
        assert (i = 0);
        Some Registers.Mask.general_r
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
    | Ptr ->
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
        Some Registers.Mask.spill
    | JmpAlways -> None
    | Jmp _ -> None
    | FunctionProlog _ -> None
    | FunctionCall _ -> None
    | FunctionCallEnd ->
        let is_void =
            match n.ir_node.typ with
            | Tuple (Value [ _; _; t ]) -> Types.equal t Void
            | _ -> false
        in
        if (not is_void) && i = 2 then
          Some Registers.Mask.rax
        else
          None
    | Param idx ->
        assert (i = 0);
        (* skip memory param *)
        if idx = 0 then
          None
        else
          Some (Registers.Mask.x64_systemv (idx - 1))
    | CalleeSave reg -> Some (Registers.Mask.of_list [ Reg reg ])
    | Return ->
        assert (i = 0);
        let is_void =
            match n.ir_node.typ with
            | Tuple (Value [ _; _; t ]) -> Types.equal t Void
            | _ -> false
        in
        if i = 0 && not is_void then Some Registers.Mask.rax else None
    | New -> if i = 1 then Some Registers.Mask.rax else None
    | Store -> None
    | Load -> Some Registers.Mask.general_w
    | Noop -> None
    | ZeroExtend ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | SignExtend ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Ideal _ -> None

let get_register_kills (n : t) =
    match n.kind with
    | FunctionCall _ -> Some Registers.Mask.caller_save
    | Div -> Some (Registers.Mask.of_list [ Reg RDX ])
    | _ -> None

let rec of_data_node g machine_g (kind : Node.data_kind) (n : Node.t) =
    let binop_commutative kind kind_imm =
        let deps = Graph.get_dependencies g n in
        match
          List.find_mapi deps ~f:(fun i n ->
              Option.bind n ~f:(fun n ->
                  match n.typ with
                  | Integer _ when Types.is_constant n.typ -> Some (i, n)
                  | _ -> None))
        with
        | None ->
            let node = { id = next_id (); kind; ir_node = n } in
            Graph.add_dependencies machine_g node [];
            Graph.add_dependencies machine_g node
              (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
            node
        | Some (idx, cn) ->
            let value =
                match cn.typ with
                | Integer _ when Types.is_constant cn.typ -> Types.get_integer_const_exn cn.typ
                | _ -> assert false (* already checked in the List.find call above *)
            in
            let kind = kind_imm value in
            let node = { id = next_id (); kind; ir_node = n } in
            Graph.add_dependencies machine_g node [];
            let deps = List.filteri deps ~f:(fun i _ -> i <> idx) in
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
            | Integer _ when Types.is_constant dep.typ ->
                let v = Types.get_integer_const_exn dep.typ in
                let kind = kind_imm v in
                let node = { id = next_id (); kind; ir_node = n } in
                Graph.add_dependencies machine_g node [];
                let deps = List.filteri deps ~f:(fun i _ -> i <> 2) in
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
            | Integer _ when Types.is_constant n.typ -> Int (Types.get_integer_const_exn n.typ)
            | Ptr _ -> Ptr
            | FunPtr _ -> Ptr
            | Struct _ -> Ptr
            | Void -> Noop
            | Bool (Value b) -> Int (Bool.to_int b |> Z.of_int)
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
                      | Integer _ when Types.is_constant n.typ -> Some (i, n)
                      | _ -> None))
            with
            | None ->
                let set_node = { id = next_id (); kind = Set m_cmp; ir_node = n } in
                let cmp_node = { id = next_id (); kind = Cmp; ir_node = n } in

                let deps = List.map cmp_deps ~f:(Option.map ~f:(convert_node g machine_g)) in
                Graph.add_dependencies machine_g cmp_node deps;
                Graph.add_dependencies machine_g set_node [ List.hd_exn deps; Some cmp_node ];
                set_node
            | Some (idx, cn) ->
                let value =
                    (* already checked in the List.find call above *)
                    Types.get_integer_const_exn cn.typ
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
                let deps = List.filteri cmp_deps ~f:(fun i _ -> i <> idx) in
                let deps = List.map deps ~f:(Option.map ~f:(convert_node g machine_g)) in
                Graph.add_dependencies machine_g cmp_node deps;
                Graph.add_dependencies machine_g set_node [ List.hd_exn deps; Some cmp_node ];
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
    | External name ->
        let node = { id = next_id (); kind = Ideal (External name); ir_node = n } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Cast ->
        let input = Graph.get_dependency g n 1 |> Option.value_exn in
        let kind =
            match input.typ with
            | Integer (Value { min; max; num_widens; fixed_width }) ->
                let is_unsigned = Z.geq min Z.zero in
                if is_unsigned then
                  ZeroExtend
                else
                  SignExtend
            | _ -> failwithf "TODO: not handled yet %s" (Types.show input.typ) ()
        in
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
    | If -> (
        let cond = Graph.get_dependency g n 1 |> Option.value_exn in
        let op =
            match cond.kind with
            | Data Eq -> Some Eq
            | Data NEq -> Some NEq
            | Data Lt -> Some Lt
            | Data LEq -> Some LEq
            | Data Gt -> Some Gt
            | Data GEq -> Some GEq
            | _ -> None
        in
        match op with
        | Some op ->
            let kind = Jmp op in
            let node = { id = next_id (); kind; ir_node = n } in
            Graph.add_dependencies machine_g node [];
            let deps =
                Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
            in
            Graph.add_dependencies machine_g node deps;
            node
        | None ->
            (* Predicate is not a compare. Add a compare node to compare the
               predicate value to 0 and set that as the depedency of the Jmp
               node *)
            let comp_node = { id = next_id (); kind = CmpImm Z.zero; ir_node = cond } in
            let kind = Jmp NEq in
            let node = { id = next_id (); kind; ir_node = n } in
            Graph.add_dependencies machine_g node [];
            let deps =
                Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
            in
            Graph.add_dependencies machine_g comp_node [ List.hd_exn deps; List.nth_exn deps 1 ];
            Graph.add_dependencies machine_g node [ List.hd_exn deps; Some comp_node ];
            node)
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
                  if Option.is_some fun_idx && Node.equal n fun_ptr then
                    None
                  else
                    Some (Some (convert_node g machine_g n)))
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
    | Store _ -> (* TODO check for ops like add that can address memory directly *) simple Store
    | Load _ -> (* TODO check for ops like add that can address memory directly *) simple Load
    | AddrOf -> failwith "todo"

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
        | Scope _ -> assert false
        | ForwardRef _ -> assert false)

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
        {
          id = next_id ();
          kind = Int Z.zero;
          ir_node = Node.create_data { filename = ""; line = 0; col = 0 } Types.ANY Constant;
        }
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
