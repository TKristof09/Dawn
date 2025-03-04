open Core

let node_to_dot_id node = Printf.sprintf "n%d" node.Node.id

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
        | Data d -> show_sexp (Node.sexp_of_data_kind d)
        | Ctrl c -> show_sexp (Node.sexp_of_ctrl_kind c)
        | Scope _ -> ""
    in
    kind_str

let to_dot g =
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
              (Printf.sprintf "  { rank = source; n%d [shape=%s,label=\"%s\",tooltip=\"%s\"]};\n"
                 node.id (node_shape node) (node_label node)
                 (Types.show_node_type node.typ
                 |> String.substr_replace_all ~pattern:"\n" ~with_:" "))
        | Ctrl Stop ->
            Buffer.add_string buf
              (Printf.sprintf "  { rank = sink; n%d [shape=%s,label=\"%s\",tooltip=\"%s\"]};\n"
                 node.id (node_shape node) (node_label node)
                 (Types.show_node_type node.typ
                 |> String.substr_replace_all ~pattern:"\n" ~with_:" "))
        | _ ->
            Buffer.add_string buf
              (Printf.sprintf "  n%d [shape=%s,label=\"%s\",tooltip=\"%s\"];\n" node.id
                 (node_shape node) (node_label node)
                 (Types.show_node_type node.typ
                 |> String.substr_replace_all ~pattern:"\n" ~with_:" ")));

    (* Second pass: Add edges *)
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Scope _ -> ()
        | _ ->
            let deps = Graph.get_dependencies g node in
            List.iter deps ~f:(fun dep ->
                let color =
                    match (dep.kind, node.kind) with
                    | Ctrl _, Data Constant -> ""
                    | Ctrl _, _ -> "color=red"
                    | _ -> ""
                in
                Buffer.add_string buf
                  (Printf.sprintf "  %s -> %s[%s];\n" (node_to_dot_id dep) (node_to_dot_id node)
                     color)));

    Buffer.add_string buf "}\n";

    (* Third pass: Add scope subgraphs *)
    Graph.iter g ~f:(fun node ->
        match node.kind with
        | Scope tbl ->
            Buffer.add_string buf (Printf.sprintf "subgraph cluster_scope_%d {\n" node.id);
            Buffer.add_string buf "  rank=sink;\n";
            Buffer.add_string buf "  label=\"Scope\";\n";

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
                      (Printf.sprintf "  sym_%d_%s -> n%d [style=dotted,arrowhead=none];\n" node.id
                         (String.hash name |> Int.to_string)
                         symbol.id)
                | None -> ())
        | _ -> ());

    Buffer.add_string buf "}\n";
    Buffer.contents buf
