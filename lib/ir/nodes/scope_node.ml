let create () = Node.create_scope ()
let ctrl_identifier = "$ctrl"

let define g (n : Node.t) name node =
    match n.kind with
    | Scope tbl ->
        Symbol_table.add_symbol tbl name node;
        Graph.add_dependencies g n [ Some node ]
    | _ -> assert false

let rec assign g (n : Node.t) name node =
    match n.kind with
    | Scope tbl ->
        let symbol = Symbol_table.find_symbol tbl name in
        let symbol =
            match symbol.kind with
            | Scope old_tbl -> (
                let tmp = Symbol_table.find_symbol old_tbl name in
                match tmp.kind with
                | Data Phi when Phi_node.get_ctrl g tmp = get_ctrl g symbol -> symbol
                | _ ->
                    let phi = Phi_node.create_half g (get_ctrl g symbol) (get g symbol name) in
                    assign g symbol name phi;
                    phi)
            | _ -> symbol
        in
        Symbol_table.reassign_symbol tbl name node;
        Graph.add_dependencies g n [ Some node ];
        Graph.remove_dependency g ~node:n ~dep:symbol
    | _ -> assert false

and get g (n : Node.t) name =
    match n.kind with
    | Scope tbl -> (
        let symbol = Symbol_table.find_symbol tbl name in
        match symbol.kind with
        | Scope old_tbl ->
            let tmp = Symbol_table.find_symbol old_tbl name in
            let symbol =
                match tmp.kind with
                | Data Phi when Phi_node.get_ctrl g tmp = get_ctrl g symbol -> symbol
                | _ ->
                    let phi = Phi_node.create_half g (get_ctrl g symbol) (get g symbol name) in
                    assign g symbol name phi;
                    phi
            in
            assign g n name symbol;
            symbol
        | _ -> symbol)
    | _ -> assert false

and get_ctrl g n = get g n ctrl_identifier

let set_ctrl g n ctrl =
    try assign g n ctrl_identifier ctrl with
    | _ -> define g n ctrl_identifier ctrl

let push (n : Node.t) =
    match n.kind with
    | Scope tbl -> n.kind <- Scope (Symbol_table.push tbl)
    | _ -> assert false

let pop g (n : Node.t) =
    match n.kind with
    | Scope tbl ->
        Symbol_table.iter_current_depth tbl (fun ~name:_ ~symbol ->
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

(* Set symbols to point to the current symbol node itself (not the duped one), that way we can easily detect the symbols when they are first used in the loop and create phi nodes for them *)
let dup_loop g (n : Node.t) =
    match n.kind with
    | Scope tbl ->
        let n_dup = Node.create_scope () in
        Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> push n_dup
            | Some _ ->
                if name = ctrl_identifier then
                  define g n_dup name (get_ctrl g n)
                else
                  define g n_dup name n);
        n_dup
    | _ -> assert false

let merge g ~(this : Node.t) ~(other : Node.t) =
    (* Same as `get` but can't have it assign the name to the phi node in the current table as we can't mutate the tabl while iterating *)
    let get_no_insert g tbl name =
        let symbol : Node.t = Symbol_table.find_symbol tbl name in
        match symbol.kind with
        | Scope _ -> (
            (* 
               This happens if the if statement is in a loop's body.
               At this point the phi node must exist as one of the branches has changed (used) this symbol.
               What can happen is that if only the "else" branch modifies a symbol, the phi node gets created in the "else" scope and the original scope (the loop's original scope, so not the loop body). Because of this we don't want to create a new phi, just get it from the original scope *)
            let node = get g symbol name in
            match node.kind with
            | Scope _ -> assert false (* This shouldn't happen i think *)
            | _ -> node)
        | _ -> symbol
    in
    match (this.kind, other.kind) with
    | Scope this_tbl, Scope other_tbl ->
        let region = Region_node.create g (get_ctrl g this, get_ctrl g other) in
        let old_ctrl = get_ctrl g this in
        let diff_fn ~name ~this:n_this ~other:_ =
            if n_this <> old_ctrl then (
              let phi =
                  Phi_node.create g region
                    [ get_no_insert g this_tbl name; get_no_insert g other_tbl name ]
              in
              Graph.add_dependencies g this [ Some phi ];
              Graph.remove_dependency g ~node:this ~dep:n_this;
              phi)
            else
              old_ctrl
        in
        Symbol_table.merge this_tbl other_tbl diff_fn Node.hard_equal;
        set_ctrl g this region;
        Graph.remove_node g other
    | _ -> assert false

let merge_loop g ~(this : Node.t) ~(body : Node.t) ~(exit : Node.t) =
    match (this.kind, body.kind, exit.kind) with
    | Scope _, Scope body_tbl, Scope exit_tbl ->
        Symbol_table.iter body_tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> ()
            | Some symbol ->
                if name <> ctrl_identifier && not (Node.hard_equal symbol this) then
                  (* Set the second input of the phi node *)
                  let n_this = get g this name in
                  let n_body = get g body name in
                  if Node.hard_equal n_this n_body then (
                    let value = Graph.get_dependency g n_this 2 |> Core.Option.value_exn in
                    Graph.replace_node_with g n_this value;
                    (* symbols get assigned from exit table to this table later *)
                    assign g exit name value)
                  else
                    Phi_node.add_input g n_this n_body);

        Symbol_table.iter exit_tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> ()
            | Some symbol ->
                if not (Node.hard_equal symbol this) then
                  assign g this name symbol);
        set_ctrl g this (get_ctrl g exit);
        Graph.remove_node g body;
        Graph.remove_node g exit
    | _ -> assert false
