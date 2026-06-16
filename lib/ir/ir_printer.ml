open Core

let node_to_dot_id id = Printf.sprintf "n%d" id

let node_shape : type a b. (a, b) Node2.t -> string =
   fun node ->
    match node.kind with
    | Data _ -> "circle"
    | Ctrl _ -> "box"
    | Scope _ -> "box"
    | Mem _ -> "box"
    | ForwardRef _ -> "box"

let node_label : type a b. (a, b) Node2.t -> string =
   fun node ->
    let kind_str =
        let show_sexp s =
            let s =
                match s with
                | Sexplib.Sexp.List (h :: _) -> Sexplib.Sexp.to_string h
                | Sexplib.Sexp.Atom a -> a
                | Sexplib.Sexp.List [] -> assert false
            in
            String.escaped s
        in
        match node.kind with
        | Data Constant -> "Const"
        | Data (Proj i)
        | Ctrl (Proj i) ->
            Printf.sprintf "Proj %d" i
        | Data (Param i) -> Printf.sprintf "Param %d" i
        | Data (Load s) -> Printf.sprintf "Load %s" s
        | Data (AddrOfField s) -> Printf.sprintf "AddrOfField %s" s
        | Mem (Store s) -> Printf.sprintf "Store %s" s
        | Data d -> show_sexp (Node2.sexp_of_data_kind (Fun.const (Sexp.Atom "")) d)
        | Ctrl c -> show_sexp (Node2.sexp_of_ctrl_kind (Fun.const (Sexp.Atom "")) c)
        | Mem m -> show_sexp (Node2.sexp_of_mem_kind (Fun.const (Sexp.Atom "")) m)
        | Scope _ -> Printf.sprintf "Scope %d" node.id
        | ForwardRef name -> Printf.sprintf "Forward ref %s" name
    in
    kind_str

let get_edge_style : type a b c d. (a, b) Node2.t -> (c, d) Node2.t -> string =
   fun def use ->
    let rec aux (def_typ : Types.t) (use_typ : Types.t) =
        match def_typ with
        | Control -> (
            match use.kind with
            | Ctrl _ -> "color=red"
            | _ -> "color=green,style=dashed,arrowhead=none")
        | DeadControl -> "color=green,style=dashed,arrowhead=none"
        | Memory -> "color=blue"
        | Tuple (Value l) -> (
            match use.kind with
            | Ctrl (Proj i)
            | Data (Proj i) ->
                aux (List.nth_exn l i) use_typ
            | Ctrl (Function _) -> "color=red"
            | Ctrl FunctionCallEnd -> ""
            | _ -> assert false)
        | _ -> ""
    in
    match (def.kind, use.kind) with
    | Ctrl (Function _), Data (Param _) -> aux def.typ use.typ
    | _, Data (Param _) ->
        let style = aux def.typ use.typ in
        if String.is_empty style then
          "style=dashed"
        else
          style ^ ",style=dashed"
    | _, _ -> aux def.typ use.typ

let pp_dot fmt g =
    Format.fprintf fmt "digraph G {\n";
    Format.fprintf fmt "splines=ortho\n";
    Format.fprintf fmt "nodesep=0.8\n";
    Format.fprintf fmt "ranksep=0.6\n";

    (* Create a subgraph for concentrated edges *)
    Format.fprintf fmt "subgraph main {@\n";
    Format.fprintf fmt "  concentrate=true;@\n";

    (* First pass: Add normal nodes *)
    let functions = Hashtbl.create (module Int) in
    Node2.G.iter g ~f:(fun (AnyNode node) ->
        match node.kind with
        | Data Constant ->
            Node2.G.get_dependants g node
            |> List.iter ~f:(fun (AnyNode dep) ->
                match dep.parent_fun with
                | None -> Hashtbl.add_multi functions ~key:0 ~data:(Node2.AnyNode node)
                | Some fun_idx -> Hashtbl.add_multi functions ~key:fun_idx ~data:(AnyNode node))
        | _ -> (
            match node.parent_fun with
            | None -> Hashtbl.add_multi functions ~key:0 ~data:(AnyNode node)
            | Some fun_idx -> Hashtbl.add_multi functions ~key:fun_idx ~data:(AnyNode node)));

    Hashtbl.iteri functions ~f:(fun ~key:fun_idx ~data:nodes ->
        Format.fprintf fmt "subgraph cluster_func_%d {\n" fun_idx;
        if fun_idx = 0 then
          Format.fprintf fmt "  label=\"Top level\";"
        else
          Format.fprintf fmt "  label=\"Function #%d\";" fun_idx;

        List.iter nodes ~f:(fun (AnyNode node) ->
            match node.kind with
            | Scope _ -> () (* Skip scopes for now *)
            | Ctrl Start ->
                Format.fprintf fmt
                  "  { rank = source; %s [shape=%s,label=\"%s\",tooltip=\"%s\"]};@\n"
                  (node_to_dot_id node.id) (node_shape node) (node_label node)
                  (Types.show node.typ
                  |> String.substr_replace_all ~pattern:"\n" ~with_:" "
                  |> String.escaped)
            | Ctrl Stop ->
                Format.fprintf fmt "  { rank = sink; %s [shape=%s,label=\"%s\",tooltip=\"%s\"]};@\n"
                  (node_to_dot_id node.id) (node_shape node) (node_label node)
                  (Types.show node.typ
                  |> String.substr_replace_all ~pattern:"\n" ~with_:" "
                  |> String.escaped)
            | Data Constant ->
                Node2.G.get_dependants g node
                |> List.filter ~f:(fun (AnyNode use) ->
                    match use.parent_fun with
                    | None -> 0 = fun_idx
                    | Some idx -> idx = fun_idx)
                |> List.iter ~f:(fun (AnyNode use) ->
                    Format.fprintf fmt "  %s_%s [shape=%s,label=\"%s\",tooltip=\"#%d: %s\"];@\n"
                      (node_to_dot_id node.id) (node_to_dot_id use.id) (node_shape node)
                      (node_label node) node.id
                      (Types.show node.typ
                      |> String.substr_replace_all ~pattern:"\n" ~with_:" "
                      |> String.escaped))
            | _ ->
                Format.fprintf fmt "  %s [shape=%s,label=\"%s\",tooltip=\"#%d: %s\"];@\n"
                  (node_to_dot_id node.id) (node_shape node) (node_label node) node.id
                  (Types.show node.typ
                  |> String.substr_replace_all ~pattern:"\n" ~with_:" "
                  |> String.escaped));
        Format.fprintf fmt "}\n");

    (* Second pass: Add edges *)
    Node2.G.iter g ~f:(fun (AnyNode node) ->
        match node.kind with
        | Scope _ -> ()
        | Data Constant -> ()
        | _ ->
            let deps = Node2.G.get_dependencies_list g node in
            List.iteri deps ~f:(fun i dep ->
                match dep with
                | None -> ()
                | Some (AnyNode dep) -> (
                    match (dep.kind, node.kind) with
                    | Ctrl Start, Ctrl (Function _) -> ()
                    | _ ->
                        let style = get_edge_style dep node in
                        let dep_id =
                            match dep.kind with
                            | Data Constant ->
                                Printf.sprintf "%s_%s" (node_to_dot_id dep.id)
                                  (node_to_dot_id node.id)
                            | _ -> node_to_dot_id dep.id
                        in
                        Format.fprintf fmt
                          "  %s -> %s[arrowsize=0.5,headlabel=%d,tooltip=\"#%d->#%d (%d)\",%s];@\n"
                          dep_id (node_to_dot_id node.id) i dep.id node.id i style)));

    Format.fprintf fmt "}@\n";

    (* Third pass: Add scope subgraphs *)
    Node2.G.iter g ~f:(fun (AnyNode node) ->
        match node.kind with
        | Scope tbl ->
            Format.fprintf fmt "subgraph cluster_scope_%d {@\n" node.id;
            Format.fprintf fmt "  rank=sink;@\n";
            Format.fprintf fmt "  label=\"%s\";@\n" (node_label node);

            (* Only create symbol nodes inside the subgraph *)
            Symbol_table.iter tbl (fun ~name ~symbol:_ ~depth:_ ->
                Format.fprintf fmt "  sym_%d_%s [label=\"%s\", shape=box];@\n" node.id
                  (String.hash name |> Int.to_string)
                  name);
            Format.fprintf fmt "}@\n";
            (* Add edges outside the subgraph definition *)
            Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
                match symbol with
                | Some symbol ->
                    let (AnyNode sym_node) = symbol.node in
                    Format.fprintf fmt "  sym_%d_%s -> %s [style=dotted,arrowhead=none];@\n" node.id
                      (String.hash name |> Int.to_string)
                      (node_to_dot_id sym_node.id)
                | None -> ())
        | _ -> ());

    Format.fprintf fmt "}@\n"

let to_dot g = Format.asprintf "%a" pp_dot g

let pp_dot_machine fmt g =
    Format.fprintf fmt "digraph G {@\n";
    Format.fprintf fmt "ordering=\"in\";@\n";
    Format.fprintf fmt "splines=ortho\n";
    Format.fprintf fmt "nodesep=0.8\n";
    Format.fprintf fmt "ranksep=0.6\n";

    (* Create a subgraph for concentrated edges *)
    Format.fprintf fmt "subgraph main {@\n";
    Format.fprintf fmt "  concentrate=true;@\n";
    Format.fprintf fmt "  style=invis;@\n";

    (* First pass: Add normal nodes *)
    Machine_node.G.iter g ~f:(fun (AnyNode node) ->
        let (AnyNode ir_node) = node.ir_node in
        match node.kind with
        | Ideal Start ->
            Format.fprintf fmt
              "  { rank = source; %s [shape=rectangle,label=\"%s\",tooltip=\"#%d (#%d)\"]};@\n"
              (node_to_dot_id node.id)
              (Machine_node.show_kind node.kind)
              node.id ir_node.id
        | Ideal Stop ->
            Format.fprintf fmt
              "  { rank = sink; %s [shape=rectangle,label=\"%s\",tooltip=\"#%d (#%d)\"]};@\n"
              (node_to_dot_id node.id)
              (Machine_node.show_kind node.kind)
              node.id ir_node.id
        | _ -> (
            match ir_node.kind with
            | Data Constant ->
                Machine_node.G.get_dependants g node
                |> List.iter ~f:(fun (AnyNode use) ->
                    Format.fprintf fmt "  %s_%s [label=\"%s\",tooltip=\"#%d (#%d)\"];@\n"
                      (node_to_dot_id node.id) (node_to_dot_id use.id)
                      (String.escaped (Machine_node.show_kind node.kind))
                      node.id ir_node.id)
            | _ ->
                Format.fprintf fmt "  %s [label=\"%s\",tooltip=\"#%d (#%d)\"];@\n"
                  (node_to_dot_id node.id)
                  (String.escaped (Machine_node.show_kind node.kind))
                  node.id ir_node.id));

    (* Second pass: Add edges *)
    Machine_node.G.iter g ~f:(fun (AnyNode node) ->
        let (AnyNode ir_node) = node.ir_node in
        match ir_node.kind with
        | Data Constant -> ()
        | _ ->
            let deps = Machine_node.G.get_dependencies_list g node in
            List.iteri deps ~f:(fun i dep ->
                match dep with
                | None -> ()
                | Some (AnyNode dep) ->
                    let (AnyNode dep_ir_node) = dep.ir_node in
                    let style =
                        match (dep.kind, node.kind) with
                        | CalleeSave _, Return -> "style=dashed,arrowhead=none"
                        | FunctionProlog _, CalleeSave _ -> "style=dashed,arrowhead=none"
                        | _ -> get_edge_style dep_ir_node ir_node
                    in
                    let dep_id =
                        match dep_ir_node.kind with
                        | Data Constant ->
                            Printf.sprintf "%s_%s" (node_to_dot_id dep.id) (node_to_dot_id node.id)
                        | _ -> node_to_dot_id dep.id
                    in

                    Format.fprintf fmt
                      "  %s -> %s[arrowsize=0.5,headlabel=%d,tooltip=\"#%d->#%d (%d)\",%s];@\n"
                      dep_id (node_to_dot_id node.id) i dep.id node.id i style));

    Format.fprintf fmt "}@\n";
    Format.fprintf fmt "}@\n"

let to_dot_machine g = Format.asprintf "%a" pp_dot_machine g

let show_node_compact g node =
    let kind_str = node_label node in
    let type_str = Types.show node.typ |> String.substr_replace_all ~pattern:"\n" ~with_:" " in
    let deps_str =
        Node2.G.get_dependencies_list g node
        |> List.tl
        |> Option.value ~default:[]
        |> List.map ~f:(function
          | None -> "_"
          | Some (AnyNode n) -> Printf.sprintf "%%%d" n.id)
        |> String.concat ~sep:", "
    in
    let deps_str = if String.is_empty deps_str then "" else "[ " ^ deps_str ^ " ]" in
    Printf.sprintf "%%%-3d: %-20s %-20s : %s" node.id kind_str deps_str type_str

(* pp helper for compact node *)
let pp_node_compact g fmt node = Format.fprintf fmt "%s" (show_node_compact g node)

(* pp version of to_string_linear *)
let pp_linear fmt g =
    Format.fprintf fmt "=== Ideal Graph (Linearized) ===@\n";

    let control_nodes =
        Node2.G.fold g ~init:[] ~f:(fun acc (AnyNode n) ->
            if Node2.is_blockhead n then Node2.AnyNode n :: acc else acc)
        |> List.sort ~compare:(fun (AnyNode a) (AnyNode b) -> Int.compare a.id b.id)
    in

    List.iter control_nodes ~f:(fun (AnyNode block) ->
        Format.fprintf fmt "Block %d (%s):@\n" block.id (Node2.show_kind block.kind);

        let dependants = Node2.G.get_dependants g block in
        List.iter dependants ~f:(fun (AnyNode n) ->
            match n.kind with
            | Data Phi -> Format.fprintf fmt "  %a@\n" (pp_node_compact g) n
            | _ -> ());

        (* Print control edges *)
        List.iter dependants ~f:(fun (AnyNode n) ->
            if Node2.is_ctrl n then
              Format.fprintf fmt "  -> %d (%s)@\n" n.id (Node2.show_kind n.kind));
        Format.fprintf fmt "@\n");

    Format.fprintf fmt "--- Floating Data Nodes ---@\n";
    Node2.G.iter g ~f:(fun (AnyNode n) ->
        if
          (not (Node2.is_ctrl n))
          &&
          match n.kind with
          | Data Phi -> false
          | _ -> true
        then
          Format.fprintf fmt "%a@\n" (pp_node_compact g) n)

let to_string_linear g = Format.asprintf "%a" pp_linear g

let show_machine_compact : type a b.
    ?reg_assoc:(Machine_node.any, Registers.loc) Base.Hashtbl.t ->
    Machine_node.G.readonly Machine_node.G.t ->
    (a, b) Machine_node.t ->
    string =
   fun ?reg_assoc g node ->
    let kind_str =
        match node.kind with
        | DProj _ -> "  |-" ^ Machine_node.show_kind node.kind
        | _ -> Machine_node.show_kind node.kind
    in
    let output_loc_str =
        match reg_assoc with
        | Some tbl -> (
            match Hashtbl.find tbl (AnyNode node) with
            | Some (Reg reg) -> Printf.sprintf "#%-5s " (Registers.show_reg reg)
            | Some (Stack offs) -> Printf.sprintf "#%-5d " offs
            | None -> "       ")
        | None -> "       "
    in
    let deps_str =
        Machine_node.G.get_dependencies_list g node
        |> List.filter_opt
        |> List.tl
        |> Option.value ~default:[]
        |> List.map ~f:(fun (AnyNode n) ->
            match reg_assoc with
            | Some tbl -> (
                match Hashtbl.find tbl (AnyNode n) with
                | Some (Reg reg) -> Printf.sprintf "#%s (%%%d)" (Registers.show_reg reg) n.id
                | Some (Stack offs) -> Printf.sprintf "$(%-3d) (%%%d)" offs n.id
                | None -> Printf.sprintf "%%%d" n.id)
            | None -> Printf.sprintf "%%%d" n.id)
        |> String.concat ~sep:", "
    in
    let (AnyNode ir_node) = node.ir_node in
    let deps_str_formatted = if String.is_empty deps_str then "" else "[ " ^ deps_str ^ " ]" in
    Printf.sprintf "%s(%%%-3d): %-15s %-45s (Ideal IR: #%d)" output_loc_str node.id kind_str
      deps_str_formatted ir_node.id

(* pp helper for compact machine node *)
let pp_machine_compact ?reg_assoc g fmt node =
    Format.fprintf fmt "%s" (show_machine_compact ?reg_assoc g node)

(* pp version of to_string_machine_linear *)
let pp_machine_linear fmt (g, program) =
    Format.fprintf fmt "=== Machine Graph (Linearized) ===@\n";

    List.iter program ~f:(fun (Machine_node.AnyNode n) ->
        if Machine_node.is_blockhead n then
          let successors =
              Machine_node.G.get_dependants g n
              |> List.filter_map ~f:(fun (AnyNode n) ->
                  if Machine_node.is_blockhead n then
                    Some (Printf.sprintf "#%d" n.id)
                  else
                    match
                      n.kind
                    with
                    | Jmp _ -> (
                        match Machine_node.G.get_dependants g n with
                        | [ AnyNode a; b ] ->
                            let AnyNode t, AnyNode f =
                                match a.kind with
                                | Ideal (CProj 0) -> (Machine_node.AnyNode a, b)
                                | _ -> (b, AnyNode a)
                            in
                            Some (Printf.sprintf "T: #%d,F: #%d" t.id f.id)
                        | [ AnyNode b ] -> (
                            match b.kind with
                            | Ideal (CProj 0) -> Some (Printf.sprintf "T: #%d,F: ___" b.id)
                            | Ideal (CProj 1) -> Some (Printf.sprintf "T: ___,F: #%d" b.id)
                            | _ -> assert false)
                        | _ -> assert false)
                    | FunctionCall _ ->
                        let (AnyNode call_end) =
                            Machine_node.G.get_dependants g n
                            |> List.find_exn ~f:(fun (AnyNode n) ->
                                match n.kind with
                                | FunctionCallEnd -> true
                                | _ -> false)
                        in
                        Some (Printf.sprintf "#%d" call_end.id)
                    | _ -> None)
              |> String.concat ~sep:", "
          in
          Format.fprintf fmt "@\nBlock #%d (%s): -> [%s]@\n" n.id (Machine_node.show_kind n.kind)
            successors
        else (
          Format.fprintf fmt "  %a@\n" (pp_machine_compact g) n;
          if Machine_node.is_multi_output n then
            Machine_node.G.get_dependants g n
            |> List.filter ~f:(fun (AnyNode n') ->
                match n'.kind with
                | DProj _ -> true
                | _ -> false)
            |> List.iter ~f:(fun (AnyNode n') ->
                Format.fprintf fmt "  %a@\n" (pp_machine_compact g) n')));
    Format.fprintf fmt "@\n"

let to_string_machine_linear g program = Format.asprintf "%a" pp_machine_linear (g, program)

(* pp version of to_string_machine_linear_regs *)
let pp_machine_linear_regs fmt (g, program, reg_assoc) =
    Format.fprintf fmt "=== Machine Graph (Linearized with registers) ===@\n";

    List.iter program ~f:(fun (Machine_node.AnyNode n) ->
        if Machine_node.is_blockhead n then
          let successors =
              Machine_node.G.get_dependants g n
              |> List.filter_map ~f:(fun (AnyNode n) ->
                  if Machine_node.is_blockhead n then
                    Some (Printf.sprintf "#%d" n.id)
                  else
                    match
                      n.kind
                    with
                    | Jmp _ -> (
                        match Machine_node.G.get_dependants g n with
                        | [ AnyNode a; b ] ->
                            let AnyNode t, AnyNode f =
                                match a.kind with
                                | Ideal (CProj 0) -> (Machine_node.AnyNode a, b)
                                | _ -> (b, AnyNode a)
                            in
                            Some (Printf.sprintf "T: #%d,F: #%d" t.id f.id)
                        | [ AnyNode b ] -> (
                            match b.kind with
                            | Ideal (CProj 0) -> Some (Printf.sprintf "T: #%d,F: ___" b.id)
                            | Ideal (CProj 1) -> Some (Printf.sprintf "T: ___,F: #%d" b.id)
                            | _ -> assert false)
                        | _ -> assert false)
                    | FunctionCall _ ->
                        let (AnyNode call_end) =
                            Machine_node.G.get_dependants g n
                            |> List.find_exn ~f:(fun (AnyNode n) ->
                                match n.kind with
                                | FunctionCallEnd -> true
                                | _ -> false)
                        in
                        Some (Printf.sprintf "#%d" call_end.id)
                    | _ -> None)
              |> String.concat ~sep:", "
          in
          Format.fprintf fmt "@\nBlock #%d (%s): -> [%s]@\n" n.id (Machine_node.show_kind n.kind)
            successors
        else (
          Format.fprintf fmt "  %a@\n" (pp_machine_compact ~reg_assoc g) n;
          if Machine_node.is_multi_output n then
            Machine_node.G.get_dependants g n
            |> List.filter ~f:(fun (AnyNode n') ->
                match n'.kind with
                | DProj _ -> true
                | _ -> false)
            |> List.iter ~f:(fun (AnyNode n') ->
                Format.fprintf fmt "  %a@\n" (pp_machine_compact ~reg_assoc g) n')));
    Format.fprintf fmt "@\n"

let to_string_machine_linear_regs g program reg_assoc =
    Format.asprintf "%a" pp_machine_linear_regs (g, program, reg_assoc)
