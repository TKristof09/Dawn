let create () = Node.create_scope ()
let ctrl_identifier = "$ctrl"
let mem_identifier = "$mem"

let rec define g (n : Node.t) name node is_const =
    let is_forward_ref =
        match node.Node.kind with
        | ForwardRef _ -> true
        | _ -> false
    in
    let variable : Node.t Variable.t = { name; node; is_const; is_forward_ref; idx = -1 } in
    match n.kind with
    | Scope tbl -> (
        match Symbol_table.find_symbol tbl name with
        | None ->
            let idx = Graph.get_dependencies g n |> List.length in
            Symbol_table.add_symbol tbl name { variable with idx };
            Graph.add_dependencies g n [ Some node ]
        | Some sym -> (
            match sym.node.kind with
            | ForwardRef fref_name ->
                assert (String.equal fref_name name);
                if not is_const then
                  Core.failwithf "Forward ref only allowed for const symbols, %s is not const" name
                    ()
                else
                  assign g n name node
            | _ ->
                (* TODO: do i want to allow shadowing? *)
                let idx = Graph.get_dependencies g n |> List.length in
                Symbol_table.add_symbol tbl name { variable with idx };
                Graph.add_dependencies g n [ Some node ]))
    | _ -> assert false

and assign g (n : Node.t) name (node : Node.t) =
    match n.kind with
    | Scope tbl -> (
        let symbol = Symbol_table.find_symbol tbl name |> Core.Option.value_exn in
        let symbol =
            match symbol.node.kind with
            | Scope old_tbl -> (
                let tmp = Symbol_table.find_symbol old_tbl name |> Core.Option.value_exn in
                match tmp.node.kind with
                | Data Phi when Phi_node.get_ctrl g tmp.node = get_ctrl g symbol.node -> symbol.node
                | _ ->
                    let phi =
                        Phi_node.create_no_backedge g node.loc (get_ctrl g symbol.node)
                          (get g symbol.node name)
                    in
                    assign g symbol.node name phi;
                    phi)
            | _ -> symbol.node
        in
        Symbol_table.reassign_symbol tbl name node;
        let idx =
            Graph.get_dependencies g n
            |> List.find_index (function
              | None -> false
              | Some x -> Node.equal x symbol)
        in
        (match idx with
        | None -> Graph.add_dependencies g n [ Some node ]
        | Some idx -> Graph.set_dependency g n (Some node) idx);
        match symbol.kind with
        | ForwardRef fref_name when String.equal fref_name name ->
            (* update uses of the forward ref to point to the actual thing *)
            (* TODO: this is kind of awful having to iterate through the entire
               symbol table to update names that point to the forward ref *)
            let is_in_symbol_table = Graph.get_dependants g symbol |> List.exists (Node.equal n) in
            if is_in_symbol_table then
              Symbol_table.iter tbl (fun ~name ~symbol:other ~depth:_ ->
                  match other with
                  | Some other ->
                      if Node.equal symbol other.node then
                        Symbol_table.reassign_symbol tbl name node
                  | None -> ());
            Graph.replace_node_with g symbol node
        | _ -> ())
    | _ -> assert false

and get g (n : Node.t) name =
    match n.kind with
    | Scope tbl -> (
        let symbol = Symbol_table.find_symbol tbl name in
        match symbol with
        | None ->
            let fref = Node.create_forward_ref name in
            define g n name fref false;
            fref
        | Some symbol -> (
            match symbol.node.kind with
            | Scope old_tbl ->
                let tmp = Symbol_table.find_symbol old_tbl name |> Core.Option.value_exn in
                let symbol =
                    match tmp.node.kind with
                    | Data Phi when Phi_node.get_ctrl g tmp.node = get_ctrl g symbol.node ->
                        tmp.node
                    | _ ->
                        let s = get g symbol.node name in
                        let phi = Phi_node.create_no_backedge g s.loc (get_ctrl g symbol.node) s in
                        assign g symbol.node name phi;
                        phi
                in
                assign g n name symbol;
                symbol
            | _ -> symbol.node))
    | _ -> assert false

and get_ctrl g n = get g n ctrl_identifier

let set_ctrl g n ctrl =
    try assign g n ctrl_identifier ctrl with
    | _ -> define g n ctrl_identifier ctrl false

let get_mem g n = get g n mem_identifier

let set_mem g n mem =
    try assign g n mem_identifier mem with
    | _ -> define g n mem_identifier mem false

let push (n : Node.t) =
    match n.kind with
    | Scope tbl -> n.kind <- Scope (Symbol_table.push tbl)
    | _ -> assert false

let pop g (n : Node.t) =
    match n.kind with
    | Scope tbl ->
        let parent_table = Symbol_table.pop tbl in
        Symbol_table.iter_current_depth tbl (fun ~name ~symbol ->
            match symbol.node.kind with
            | ForwardRef _ ->
                (* bubble up forward ref to parent scope *)
                Symbol_table.add_symbol parent_table name symbol
            | _ -> Graph.remove_dependency g ~node:n ~dep:symbol.node);
        n.kind <- Scope parent_table
    | _ -> assert false

let dup g (n : Node.t) =
    match n.kind with
    | Scope tbl ->
        let n_dup = Node.create_scope () in
        Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> push n_dup
            | Some symbol -> define g n_dup name symbol.node symbol.is_const);
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
            | Some symbol ->
                if name = ctrl_identifier then
                  define g n_dup name (get_ctrl g n) symbol.is_const
                else if symbol.is_const then
                  define g n_dup name symbol.node symbol.is_const
                else
                  define g n_dup name n symbol.is_const);
        n_dup
    | _ -> assert false

let merge g loc ~(this : Node.t) ~(other : Node.t) =
    (* Same as `get` but can't have it assign the name to the phi node in the current table as we can't mutate the tabl while iterating *)
    let get_no_insert g tbl name =
        let symbol : Node.t Variable.t =
            Symbol_table.find_symbol tbl name |> Core.Option.value_exn
        in
        match symbol.node.kind with
        | Scope _ -> (
            (* 
               This happens if the if statement is in a loop's body.
               At this point the phi node must exist as one of the branches has changed (used) this symbol.
               What can happen is that if only the "else" branch modifies a symbol, the phi node gets created in the "else" scope and the original scope (the loop's original scope, so not the loop body). Because of this we don't want to create a new phi, just get it from the original scope *)
            let node = get g symbol.node name in
            match node.kind with
            | Scope _ -> assert false (* This shouldn't happen i think *)
            | _ -> node)
        | _ -> symbol.node
    in
    match (this.kind, other.kind) with
    | Scope this_tbl, Scope other_tbl ->
        let region = Region_node.create g loc (get_ctrl g this, get_ctrl g other) in
        let old_ctrl = get_ctrl g this in
        let diff_fn ~name ~this:(v_this : Node.t Variable.t) ~other:_ =
            if not (Node.equal v_this.node old_ctrl) then (
              let phi =
                  Phi_node.create g loc region
                    [ get_no_insert g this_tbl name; get_no_insert g other_tbl name ]
              in
              Graph.add_dependencies g this [ Some phi ];
              Graph.remove_dependency g ~node:this ~dep:v_this.node;
              { v_this with node = phi })
            else
              { v_this with node = old_ctrl }
        in
        Symbol_table.merge this_tbl other_tbl diff_fn (fun v v' -> Node.equal v.node v'.node);
        set_ctrl g this region;
        Graph.remove_node g other
    | _ -> assert false

let merge_loop g ~(this : Node.t) ~(body : Node.t) ~(exit : Node.t) =
    match (this.kind, body.kind, exit.kind) with
    | Scope this_tbl, Scope body_tbl, Scope exit_tbl ->
        let phis = ref [] in
        Symbol_table.iter body_tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> ()
            | Some symbol ->
                (* Set the second input of the phi node *)
                if
                  name <> ctrl_identifier
                  && (not symbol.is_const)
                  && not (Node.equal symbol.node this)
                then
                  let v_this = Symbol_table.find_symbol this_tbl name |> Core.Option.value_exn in
                  let v_body = Symbol_table.find_symbol body_tbl name |> Core.Option.value_exn in
                  if Node.equal v_this.node v_body.node then (
                    let value = Graph.get_dependency g v_this.node 2 |> Core.Option.value_exn in
                    Graph.replace_node_with g v_this.node value;
                    (* symbols get assigned from exit table to this table later *)
                    Symbol_table.reassign_symbol exit_tbl name value)
                  else (
                    phis := v_this.node :: !phis;
                    Phi_node.add_backedge_input g v_this.node v_body.node));

        (* TODO: we put phis to bottom type for now. we'll have constant propagation that calculates better type later in SCCP *)
        Core.List.iter !phis ~f:(fun n -> n.typ <- ALL);
        Symbol_table.iter exit_tbl (fun ~name ~symbol ~depth:_ ->
            match symbol with
            | None -> ()
            | Some symbol ->
                if
                  name <> ctrl_identifier
                  && (not symbol.is_const)
                  && not (Node.equal symbol.node this)
                then
                  Symbol_table.reassign_symbol this_tbl name symbol.node);
        set_ctrl g this (get_ctrl g exit);
        Graph.remove_node g body;
        Graph.remove_node g exit
    | _ -> assert false
