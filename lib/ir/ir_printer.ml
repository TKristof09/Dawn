open Core

let node_to_dot_id id = Printf.sprintf "n%d" id

let node_shape (node : Node.t) =
    match node.kind with
    | Data _ -> "circle"
    | Ctrl _ -> "box"
    | Scope _ -> "box"
    | Mem _ -> "box"
    | ForwardRef _ -> "box"

let node_label node =
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
        match node.Node.kind with
        | Data Constant -> "Const"
        | Data (Proj i)
        | Ctrl (Proj i) ->
            Printf.sprintf "Proj %d" i
        | Data (Param i) -> Printf.sprintf "Param %d" i
        | Mem (Load s) -> Printf.sprintf "Load %s" s
        | Mem (Store s) -> Printf.sprintf "Store %s" s
        | Mem (AddrOfField s) -> Printf.sprintf "AddrOfField %s" s
        | Data d -> show_sexp (Node.sexp_of_data_kind d)
        | Ctrl c -> show_sexp (Node.sexp_of_ctrl_kind c)
        | Mem m -> show_sexp (Node.sexp_of_mem_kind m)
        | Scope _ -> Printf.sprintf "Scope %d" node.id
        | ForwardRef name -> Printf.sprintf "Forward ref %s" name
    in
    kind_str

let get_edge_style (def : Node.t) (use : Node.t) =
    let rec aux (def_typ : Types.t) (use_typ : Types.t) =
        match def_typ with
        | Control -> (
            match use.kind with
            | Ctrl _ -> "color=red"
            | _ -> "color=green,style=dashed,arrowhead=none")
        | DeadControl -> "color=green,style=dashed,arrowhead=none"
        | Memory -> "color=blue"
        | Tuple (Value l) -> (
            if Types.equal def_typ use_typ then
              ""
            else
              match
                use.kind
              with
              | Ctrl (Proj i)
              | Data (Proj i) ->
                  aux (List.nth_exn l i) use_typ
              | Ctrl (Function _) -> "color=red"
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
    Graph.iter g ~f:(fun node ->
        match node.Node.kind with
        | Data Constant ->
            Graph.get_dependants g node
            |> List.iter ~f:(fun dep ->
                match dep.parent_fun with
                | None -> Hashtbl.add_multi functions ~key:0 ~data:node
                | Some fun_idx -> Hashtbl.add_multi functions ~key:fun_idx ~data:node)
        | _ -> (
            match node.parent_fun with
            | None -> Hashtbl.add_multi functions ~key:0 ~data:node
            | Some fun_idx -> Hashtbl.add_multi functions ~key:fun_idx ~data:node));

    Hashtbl.iteri functions ~f:(fun ~key:fun_idx ~data:nodes ->
        Format.fprintf fmt "subgraph cluster_func_%d {\n" fun_idx;
        if fun_idx = 0 then
          Format.fprintf fmt "  label=\"Top level\";"
        else
          Format.fprintf fmt "  label=\"Function #%d\";" fun_idx;

        List.iter nodes ~f:(fun (node : Node.t) ->
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
                Graph.get_dependants g node
                |> List.filter ~f:(fun use ->
                    match use.parent_fun with
                    | None -> 0 = fun_idx
                    | Some idx -> idx = fun_idx)
                |> List.iter ~f:(fun use ->
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
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Scope _ -> ()
        | Data Constant -> ()
        | _ ->
            let deps = Graph.get_dependencies g node in
            List.iteri deps ~f:(fun i dep ->
                match dep with
                | None -> ()
                | Some dep -> (
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
    Graph.iter g ~f:(fun node ->
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
                    Format.fprintf fmt "  sym_%d_%s -> %s [style=dotted,arrowhead=none];@\n" node.id
                      (String.hash name |> Int.to_string)
                      (node_to_dot_id symbol.node.id)
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
    Graph.iter g ~f:(fun (node : Machine_node.t) ->
        match node.kind with
        | Ideal Start ->
            Format.fprintf fmt
              "  { rank = source; %s [shape=rectangle,label=\"%s\",tooltip=\"#%d (#%d)\"]};@\n"
              (node_to_dot_id node.id)
              (Machine_node.show_machine_node_kind node.kind)
              node.id node.ir_node.id
        | Ideal Stop ->
            Format.fprintf fmt
              "  { rank = sink; %s [shape=rectangle,label=\"%s\",tooltip=\"#%d (#%d)\"]};@\n"
              (node_to_dot_id node.id)
              (Machine_node.show_machine_node_kind node.kind)
              node.id node.ir_node.id
        | _ -> (
            match node.ir_node.kind with
            | Data Constant ->
                Graph.get_dependants g node
                |> List.iter ~f:(fun use ->
                    Format.fprintf fmt "  %s_%s [label=\"%s\",tooltip=\"#%d (#%d)\"];@\n"
                      (node_to_dot_id node.id) (node_to_dot_id use.id)
                      (String.escaped (Machine_node.show_machine_node_kind node.kind))
                      node.id node.ir_node.id)
            | _ ->
                Format.fprintf fmt "  %s [label=\"%s\",tooltip=\"#%d (#%d)\"];@\n"
                  (node_to_dot_id node.id)
                  (String.escaped (Machine_node.show_machine_node_kind node.kind))
                  node.id node.ir_node.id));

    (* Second pass: Add edges *)
    Graph.iter g ~f:(fun node ->
        match node.ir_node.kind with
        | Data Constant -> ()
        | _ ->
            let deps = Graph.get_dependencies g node in
            List.iteri deps ~f:(fun i dep ->
                match dep with
                | None -> ()
                | Some dep ->
                    let style =
                        match (dep.kind, node.kind) with
                        | CalleeSave _, Return -> "style=dashed,arrowhead=none"
                        | FunctionProlog _, CalleeSave _ -> "style=dashed,arrowhead=none"
                        | _ -> get_edge_style dep.ir_node node.ir_node
                    in
                    let dep_id =
                        match dep.ir_node.kind with
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

let show_node_compact g (node : Node.t) =
    let kind_str = node_label node in
    let type_str = Types.show node.typ |> String.substr_replace_all ~pattern:"\n" ~with_:" " in
    let deps_str =
        Graph.get_dependencies g node
        |> List.tl
        |> Option.value ~default:[]
        |> List.map ~f:(function
          | None -> "_"
          | Some n -> Printf.sprintf "%%%d" n.id)
        |> String.concat ~sep:", "
    in
    let deps_str = if String.is_empty deps_str then "" else "[ " ^ deps_str ^ " ]" in
    Printf.sprintf "%%%-3d: %-20s %-20s : %s" node.id kind_str deps_str type_str

(* pp helper for compact node *)
let pp_node_compact g fmt (node : Node.t) = Format.fprintf fmt "%s" (show_node_compact g node)

(* pp version of to_string_linear *)
let pp_linear fmt g =
    Format.fprintf fmt "=== Ideal Graph (Linearized) ===@\n";

    let control_nodes =
        Graph.fold g ~init:[] ~f:(fun acc n -> if Node.is_blockhead n then n :: acc else acc)
        |> List.sort ~compare:(fun a b -> Int.compare a.id b.id)
    in

    List.iter control_nodes ~f:(fun block ->
        Format.fprintf fmt "Block %d (%s):@\n" block.id (Node.show_kind block.kind);

        let dependants = Graph.get_dependants g block in
        List.iter dependants ~f:(fun n ->
            match n.kind with
            | Data Phi -> Format.fprintf fmt "  %a@\n" (pp_node_compact g) n
            | _ -> ());

        (* Print control edges *)
        List.iter dependants ~f:(fun n ->
            if Node.is_ctrl n then
              Format.fprintf fmt "  -> %d (%s)@\n" n.id (Node.show_kind n.kind));
        Format.fprintf fmt "@\n");

    Format.fprintf fmt "--- Floating Data Nodes ---@\n";
    Graph.iter g ~f:(fun n ->
        if
          (not (Node.is_ctrl n))
          &&
          match n.kind with
          | Data Phi -> false
          | _ -> true
        then
          Format.fprintf fmt "%a@\n" (pp_node_compact g) n)

let to_string_linear g = Format.asprintf "%a" pp_linear g

let show_machine_compact ?(reg_assoc : (Machine_node.t, Registers.loc) Base.Hashtbl.t option) g
    (node : Machine_node.t) =
    let kind_str =
        match node.kind with
        | DProj _ -> "  |-" ^ Machine_node.show_machine_node_kind node.kind
        | _ -> Machine_node.show_machine_node_kind node.kind
    in
    let output_loc_str =
        match reg_assoc with
        | Some tbl -> (
            match Hashtbl.find tbl node with
            | Some (Reg reg) -> Printf.sprintf "#%-5s " (Registers.show_reg reg)
            | Some (Stack offs) -> Printf.sprintf "#%-5d " offs
            | None -> "       ")
        | None -> "       "
    in
    let deps_str =
        Graph.get_dependencies g node
        |> List.filter_opt
        |> List.tl
        |> Option.value ~default:[]
        |> List.map ~f:(fun n ->
            match reg_assoc with
            | Some tbl -> (
                match Hashtbl.find tbl n with
                | Some (Reg reg) -> Printf.sprintf "#%s (%%%d)" (Registers.show_reg reg) n.id
                | Some (Stack offs) -> Printf.sprintf "$(%-3d) (%%%d)" offs n.id
                | None -> Printf.sprintf "%%%d" n.id)
            | None -> Printf.sprintf "%%%d" n.id)
        |> String.concat ~sep:", "
    in
    let deps_str_formatted = if String.is_empty deps_str then "" else "[ " ^ deps_str ^ " ]" in
    Printf.sprintf "%s(%%%-3d): %-15s %-45s (Ideal IR: #%d)" output_loc_str node.id kind_str
      deps_str_formatted node.ir_node.id

(* pp helper for compact machine node *)
let pp_machine_compact ?reg_assoc g fmt (node : Machine_node.t) =
    Format.fprintf fmt "%s" (show_machine_compact ?reg_assoc g node)

(* pp version of to_string_machine_linear *)
let pp_machine_linear fmt (g, program) =
    Format.fprintf fmt "=== Machine Graph (Linearized) ===@\n";

    List.iter program ~f:(fun n ->
        if Machine_node.is_blockhead n then
          let successors =
              Graph.get_dependants g n
              |> List.filter_map ~f:(fun n ->
                  if Machine_node.is_blockhead n then
                    Some (Printf.sprintf "#%d" n.id)
                  else
                    match
                      n.kind
                    with
                    | Jmp _ -> (
                        match Graph.get_dependants g n with
                        | [ a; b ] ->
                            let t, f =
                                if Poly.equal a.kind (Ideal (CProj 0)) then (a, b) else (b, a)
                            in
                            Some (Printf.sprintf "T: #%d,F: #%d" t.id f.id)
                        | [ b ] -> (
                            match b.kind with
                            | Ideal (CProj 0) -> Some (Printf.sprintf "T: #%d,F: ___" b.id)
                            | Ideal (CProj 1) -> Some (Printf.sprintf "T: ___,F: #%d" b.id)
                            | _ -> assert false)
                        | _ -> assert false)
                    | FunctionCall _ ->
                        let call_end =
                            Graph.get_dependants g n
                            |> List.find_exn ~f:(fun n ->
                                match n.kind with
                                | FunctionCallEnd -> true
                                | _ -> false)
                        in
                        Some (Printf.sprintf "#%d" call_end.id)
                    | _ -> None)
              |> String.concat ~sep:", "
          in
          Format.fprintf fmt "@\nBlock #%d (%s): -> [%s]@\n" n.id
            (Machine_node.show_machine_node_kind n.kind)
            successors
        else (
          Format.fprintf fmt "  %a@\n" (pp_machine_compact g) n;
          if Machine_node.is_multi_output n then
            Graph.get_dependants g n
            |> List.filter ~f:(fun n' ->
                match n'.kind with
                | DProj _ -> true
                | _ -> false)
            |> List.iter ~f:(fun n' -> Format.fprintf fmt "  %a@\n" (pp_machine_compact g) n')));
    Format.fprintf fmt "@\n"

let to_string_machine_linear g program = Format.asprintf "%a" pp_machine_linear (g, program)

(* pp version of to_string_machine_linear_regs *)
let pp_machine_linear_regs fmt (g, program, reg_assoc) =
    Format.fprintf fmt "=== Machine Graph (Linearized with registers) ===@\n";

    List.iter program ~f:(fun n ->
        if Machine_node.is_blockhead n then
          let successors =
              Graph.get_dependants g n
              |> List.filter_map ~f:(fun n ->
                  if Machine_node.is_blockhead n then
                    Some (Printf.sprintf "#%d" n.id)
                  else
                    match
                      n.kind
                    with
                    | Jmp _ -> (
                        match Graph.get_dependants g n with
                        | [ a; b ] ->
                            let t, f =
                                if Poly.equal a.kind (Ideal (CProj 0)) then (a, b) else (b, a)
                            in
                            Some (Printf.sprintf "T: #%d,F: #%d" t.id f.id)
                        | [ b ] -> (
                            match b.kind with
                            | Ideal (CProj 0) -> Some (Printf.sprintf "T: #%d,F: ___" b.id)
                            | Ideal (CProj 1) -> Some (Printf.sprintf "T: ___,F: #%d" b.id)
                            | _ -> assert false)
                        | _ -> assert false)
                    | FunctionCall _ ->
                        let call_end =
                            Graph.get_dependants g n
                            |> List.find_exn ~f:(fun n ->
                                match n.kind with
                                | FunctionCallEnd -> true
                                | _ -> false)
                        in
                        Some (Printf.sprintf "#%d" call_end.id)
                    | _ -> None)
              |> String.concat ~sep:", "
          in
          Format.fprintf fmt "@\nBlock #%d (%s): -> [%s]@\n" n.id
            (Machine_node.show_machine_node_kind n.kind)
            successors
        else (
          Format.fprintf fmt "  %a@\n" (pp_machine_compact ~reg_assoc g) n;
          if Machine_node.is_multi_output n then
            Graph.get_dependants g n
            |> List.filter ~f:(fun n' ->
                match n'.kind with
                | DProj _ -> true
                | _ -> false)
            |> List.iter ~f:(fun n' ->
                Format.fprintf fmt "  %a@\n" (pp_machine_compact ~reg_assoc g) n')));
    Format.fprintf fmt "@\n"

let to_string_machine_linear_regs g program reg_assoc =
    Format.asprintf "%a" pp_machine_linear_regs (g, program, reg_assoc)
