open Core

let node_to_dot_id id = Printf.sprintf "n%d" id

let node_shape (node : Node.t) =
    match node.kind with
    | Node.Data _ -> "circle"
    | Node.Ctrl _ -> "box"
    | Node.Scope _ -> "box"

let node_label node =
    let kind_str =
        let show_sexp s =
            match s with
            | Sexplib.Sexp.List (h :: _) -> Sexplib.Sexp.to_string h
            | Sexplib.Sexp.Atom a -> a
            | Sexplib.Sexp.List [] -> assert false
        in
        match node.Node.kind with
        | Data Constant -> "Const"
        | Data (Proj i)
        | Ctrl (Proj i) ->
            Printf.sprintf "Proj %d" i
        | Data d -> show_sexp (Node.sexp_of_data_kind d)
        | Ctrl c -> show_sexp (Node.sexp_of_ctrl_kind c)
        | Scope _ -> Printf.sprintf "Scope %d" node.id
    in
    kind_str

let to_dot (g : Node.t Graph.t) =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "digraph G {\n";
    Buffer.add_string buf "ordering=\"in\";\n";

    (*Buffer.add_string buf "concentrate=\"true\";\n";*)

    (* Create a subgraph for concentrated edges *)
    Buffer.add_string buf "subgraph main {\n";
    Buffer.add_string buf "  concentrate=true;\n";
    Buffer.add_string buf "  style=invis;\n";

    (* First pass: Add normal nodes *)
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Scope _ -> () (* Skip scopes for now *)
        | Ctrl Start ->
            Buffer.add_string buf
              (Printf.sprintf "  { rank = source; %s [shape=%s,label=\"%s\",tooltip=\"%s\"]};\n"
                 (node_to_dot_id node.id) (node_shape node) (node_label node)
                 (Types.show_node_type node.typ
                 |> String.substr_replace_all ~pattern:"\n" ~with_:" "))
        | Ctrl Stop ->
            Buffer.add_string buf
              (Printf.sprintf "  { rank = sink; %s [shape=%s,label=\"%s\",tooltip=\"%s\"]};\n"
                 (node_to_dot_id node.id) (node_shape node) (node_label node)
                 (Types.show_node_type node.typ
                 |> String.substr_replace_all ~pattern:"\n" ~with_:" "))
        | _ ->
            Buffer.add_string buf
              (Printf.sprintf "  %s [shape=%s,label=\"%s\",tooltip=\"#%d: %s\"];\n"
                 (node_to_dot_id node.id) (node_shape node) (node_label node) node.id
                 (Types.show_node_type node.typ
                 |> String.substr_replace_all ~pattern:"\n" ~with_:" ")));

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
                | Some dep ->
                    let style =
                        match (dep.kind, node.kind) with
                        | Ctrl _, Data Constant -> ""
                        | Ctrl _, Data _ -> "color=green,style=dashed,arrowhead=none"
                        | Ctrl _, _ -> "color=red"
                        | _ -> ""
                    in

                    Buffer.add_string buf
                      (Printf.sprintf "  %s -> %s[arrowsize=0.5,headlabel=%d,%s];\n"
                         (node_to_dot_id dep.id) (node_to_dot_id node.id) i style)));

    Buffer.add_string buf "}\n";

    (* Third pass: Add scope subgraphs *)
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Scope tbl ->
            Buffer.add_string buf (Printf.sprintf "subgraph cluster_scope_%d {\n" node.id);
            Buffer.add_string buf "  rank=sink;\n";
            Buffer.add_string buf (Printf.sprintf "  label=\"%s\";\n" (node_label node));

            (* Only create symbol nodes inside the subgraph *)
            Symbol_table.iter tbl (fun ~name ~symbol:_ ~depth:_ ->
                Buffer.add_string buf
                  (Printf.sprintf "  sym_%d_%s [label=\"%s\", shape=box];\n" node.id
                     (String.hash name |> Int.to_string)
                     name));
            Buffer.add_string buf "}\n";
            (* Add edges outside the subgraph definition *)
            Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
                match symbol with
                | Some symbol ->
                    Buffer.add_string buf
                      (Printf.sprintf "  sym_%d_%s -> %s [style=dotted,arrowhead=none];\n" node.id
                         (String.hash name |> Int.to_string)
                         (node_to_dot_id symbol.id))
                | None -> ())
        | _ -> ());

    Buffer.add_string buf "}\n";
    Buffer.contents buf

let to_dot_machine (g : Machine_node.t Graph.t) =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "digraph G {\n";
    Buffer.add_string buf "ordering=\"in\";\n";

    (*Buffer.add_string buf "concentrate=\"true\";\n";*)

    (* Create a subgraph for concentrated edges *)
    Buffer.add_string buf "subgraph main {\n";
    Buffer.add_string buf "  concentrate=true;\n";
    Buffer.add_string buf "  style=invis;\n";

    (* First pass: Add normal nodes *)
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Ideal Start ->
            Buffer.add_string buf
              (Printf.sprintf
                 "  { rank = source; %s [shape=rectangle,label=\"%s\",tooltip=\"#%d (#%d)\"]};\n"
                 (node_to_dot_id node.id)
                 (Machine_node.show_machine_node_kind node.kind)
                 node.id node.ir_node.id)
        | Ideal Stop ->
            Buffer.add_string buf
              (Printf.sprintf
                 "  { rank = sink; %s [shape=rectangle,label=\"%s\",tooltip=\"#%d (#%d)\"]};\n"
                 (node_to_dot_id node.id)
                 (Machine_node.show_machine_node_kind node.kind)
                 node.id node.ir_node.id)
        | _ ->
            Buffer.add_string buf
              (Printf.sprintf "  %s [label=\"%s\",tooltip=\"#%d (#%d)\"];\n"
                 (node_to_dot_id node.id)
                 (Machine_node.show_machine_node_kind node.kind)
                 node.id node.ir_node.id));

    (* Second pass: Add edges *)
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Int _ -> ()
        | _ ->
            let deps = Graph.get_dependencies g node in
            List.iteri deps ~f:(fun i dep ->
                match dep with
                | None -> ()
                | Some dep ->
                    let style =
                        match (dep.kind, node.kind) with
                        | _, Int _ when Machine_node.is_control_node dep -> ""
                        | _, _
                          when Machine_node.is_control_node dep
                               && not (Machine_node.is_control_node node) ->
                            "color=green,style=dashed,arrowhead=none"
                        | _, _ when Machine_node.is_control_node dep -> "color=red"
                        | _ -> ""
                    in

                    Buffer.add_string buf
                      (Printf.sprintf "  %s -> %s[arrowsize=0.5,headlabel=%d,%s];\n"
                         (node_to_dot_id dep.id) (node_to_dot_id node.id) i style)));

    Buffer.add_string buf "}\n";
    Buffer.add_string buf "}\n";
    Buffer.contents buf

let show_node_compact g (node : Node.t) =
    let kind_str = node_label node in
    let type_str =
        Types.show_node_type node.typ |> String.substr_replace_all ~pattern:"\n" ~with_:" "
    in
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

let to_string_linear (g : Node.t Graph.t) =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "=== Ideal Graph (Linearized) ===\n";

    let control_nodes =
        Graph.fold g ~init:[] ~f:(fun acc n -> if Node.is_blockhead n then n :: acc else acc)
        |> List.sort ~compare:(fun a b -> Int.compare a.id b.id)
    in

    List.iter control_nodes ~f:(fun block ->
        Buffer.add_string buf
          (* TODO: show block successors like in the machine version *)
          (Printf.sprintf "Block %d (%s):\n" block.id (Node.show_kind block.kind));

        let dependants = Graph.get_dependants g block in
        List.iter dependants ~f:(fun n ->
            match n.kind with
            | Data Phi -> Buffer.add_string buf (Printf.sprintf "  %s\n" (show_node_compact g n))
            | _ -> ());

        (* Print control edges *)
        List.iter dependants ~f:(fun n ->
            if Node.is_ctrl n then
              Buffer.add_string buf (Printf.sprintf "  -> %d (%s)\n" n.id (Node.show_kind n.kind)));
        Buffer.add_string buf "\n");

    Buffer.add_string buf "--- Floating Data Nodes ---\n";
    Graph.iter g ~f:(fun n ->
        if
          (not (Node.is_ctrl n))
          &&
          match n.kind with
          | Data Phi -> false
          | _ -> true
        then
          Buffer.add_string buf (Printf.sprintf "%s\n" (show_node_compact g n)));
    Buffer.contents buf

let show_machine_compact ?(reg_assoc : (Machine_node.t, Registers.reg) Hashtbl.t option) g
    (node : Machine_node.t) =
    let kind_str = Machine_node.show_machine_node_kind node.kind in
    let output_reg_str =
        match reg_assoc with
        | Some tbl -> (
            match Hashtbl.find tbl node with
            | Some reg -> Printf.sprintf "#%-5s " (Registers.show_reg reg)
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
                   | Some reg -> Printf.sprintf "#%s (%%%d)" (Registers.show_reg reg) n.id
                   | None -> Printf.sprintf "%%%d" n.id)
               | None -> Printf.sprintf "%%%d" n.id)
        |> String.concat ~sep:", "
    in
    let deps_str_formatted = if String.is_empty deps_str then "" else "[ " ^ deps_str ^ " ]" in
    Printf.sprintf "%s(%%%-3d): %-20s %-30s (Ideal IR: #%d)" output_reg_str node.id kind_str
      deps_str_formatted node.ir_node.id

let to_string_machine_linear (g : Machine_node.t Graph.t) (program : Machine_node.t list) =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "=== Machine Graph (Linearized) ===\n";

    List.iter program ~f:(fun n ->
        if Machine_node.is_blockhead n then
          let successors =
              Graph.get_dependants g n
              |> List.filter_map ~f:(fun n ->
                     if Machine_node.is_blockhead n then
                       Some (Printf.sprintf "#%d" n.id)
                     else
                       match n.kind with
                       | Jmp _ ->
                           let t, f =
                               match Graph.get_dependants g n with
                               | [ t; f ] -> (t, f)
                               | _ -> assert false
                           in

                           Some (Printf.sprintf "T: #%d,F: #%d" t.id f.id)
                       | _ -> None)
              |> String.concat ~sep:", "
          in
          Buffer.add_string buf
            (Printf.sprintf "\nBlock #%d (%s): -> [%s]\n" n.id
               (Machine_node.show_machine_node_kind n.kind)
               successors)
        else
          Buffer.add_string buf (Printf.sprintf "  %s\n" (show_machine_compact g n)));
    Buffer.add_string buf "\n";
    Buffer.contents buf

let to_string_machine_linear_regs (g : Machine_node.t Graph.t) (program : Machine_node.t list)
    (reg_assoc : (Machine_node.t, Registers.reg) Hashtbl.t) =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "=== Machine Graph (Linearized with registers) ===\n";

    List.iter program ~f:(fun n ->
        if Machine_node.is_blockhead n then
          let successors =
              Graph.get_dependants g n
              |> List.filter_map ~f:(fun n ->
                     if Machine_node.is_blockhead n then
                       Some (Printf.sprintf "#%d" n.id)
                     else
                       match n.kind with
                       | Jmp _ ->
                           let t, f =
                               match Graph.get_dependants g n with
                               | [ t; f ] -> (t, f)
                               | _ -> assert false
                           in

                           Some (Printf.sprintf "T: #%d,F: #%d" t.id f.id)
                       | _ -> None)
              |> String.concat ~sep:", "
          in
          Buffer.add_string buf
            (Printf.sprintf "\nBlock #%d (%s): -> [%s]\n" n.id
               (Machine_node.show_machine_node_kind n.kind)
               successors)
        else
          Buffer.add_string buf (Printf.sprintf "  %s\n" (show_machine_compact ~reg_assoc g n)));
    Buffer.add_string buf "\n";
    Buffer.contents buf
