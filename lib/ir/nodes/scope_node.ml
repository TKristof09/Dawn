let create () = Node.create_scope ()

let define g (n : Node.t) name node =
    match n.kind with
    | Scope tbl ->
        Symbol_table.add_symbol tbl name node;
        Graph.add_dependencies g n [ node ]
    | _ -> assert false

let assign g (n : Node.t) name node =
    match n.kind with
    | Scope tbl ->
        let old_symbol = Symbol_table.find_symbol tbl name in
        Symbol_table.reassign_symbol tbl name node;
        Printf.printf "Reassign %s\n" name;
        Graph.add_dependencies g n [ node ];
        Graph.remove_dependency g ~node:n ~dep:old_symbol
    | _ -> assert false

let get (n : Node.t) name =
    match n.kind with
    | Scope tbl -> Symbol_table.find_symbol tbl name
    | _ -> assert false

let push (n : Node.t) =
    match n.kind with
    | Scope tbl -> n.kind <- Scope (Symbol_table.push tbl)
    | _ -> assert false

let pop g (n : Node.t) =
    match n.kind with
    | Scope tbl ->
        Symbol_table.iter_current_depth tbl (fun ~name ~symbol ->
            Printf.printf "Removing %s\n" name;
            Graph.remove_dependency g ~node:n ~dep:symbol);
        n.kind <- Scope (Symbol_table.pop tbl)
    | _ -> assert false

let dup g (n : Node.t) =
    match n.kind with
    | Scope tbl ->
        let n_dup = Node.create_scope () in
        Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> push n_dup
            | Some symbol -> define g n_dup name symbol);
        n_dup
    | _ -> assert false

let ctrl_identifier = "$ctrl"

let set_ctrl g n ctrl =
    try assign g n ctrl_identifier ctrl with
    | _ -> define g n ctrl_identifier ctrl

let get_ctrl n = get n ctrl_identifier

let merge g ~(this : Node.t) ~(other : Node.t) =
    match (this.kind, other.kind) with
    | Scope this_tbl, Scope other_tbl ->
        let region = Region_node.create g (get_ctrl this, get_ctrl other) in
        let old_ctrl = get_ctrl this in
        let diff_fn ~this:n_this ~other:n_other =
            if n_this <> old_ctrl then (
              let phi = Phi_node.create g region [ n_this; n_other ] in
              Graph.add_dependencies g this [ phi ];
              Graph.remove_dependency g ~node:this ~dep:n_this;
              phi)
            else
              old_ctrl
        in
        Symbol_table.merge this_tbl other_tbl diff_fn Node.equal;
        set_ctrl g this region;
        Graph.remove_node g other
    | _ -> assert false
