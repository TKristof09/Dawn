open Core

let create () = Node2.create_scope ()
let ctrl_identifier = "$ctrl"
let mem_identifier = "$mem"

let rec define : type a b.
    Node2.G.readwrite Node2.G.t ->
    (Node2.scope_kind, Node2.misc) Node2.t ->
    string ->
    (a, b) Node2.t ->
    bool ->
    unit =
   fun g scope name node is_const ->
    let is_forward_ref =
        match node.Node2.kind with
        | ForwardRef _ -> true
        | _ -> false
    in
    let variable =
        { Variable.name; node = Node2.AnyNode node; is_const; is_forward_ref; idx = -1 }
    in
    let (Node2.Scope tbl) = scope.Node2.kind in
    match Symbol_table.find_symbol tbl name with
    | None ->
        let { Node2.vars } = Node2.G.get_dependencies_exn g scope in
        let idx = List.length vars in
        Symbol_table.add_symbol tbl name { variable with idx };
        Node2.G.set_node_inputs g scope { Node2.vars = vars @ [ Some (AnyNode node) ] }
    | Some { name = _; node = AnyNode n'; is_const = _; is_forward_ref; idx = _ } -> (
        match n'.kind with
        | ForwardRef fref_name ->
            assert (String.equal fref_name name);
            if not is_const then
              Core.failwithf "Forward ref only allowed for const symbols, %s is not const" name ()
            else
              assign g scope name node
        | _ ->
            (* TODO: do i want to allow shadowing? *)
            let { Node2.vars } = Node2.G.get_dependencies_exn g scope in
            let idx = List.length vars in
            Symbol_table.add_symbol tbl name { variable with idx };
            Node2.G.set_node_inputs g scope { Node2.vars = vars @ [ Some (AnyNode node) ] })

and assign : type a b.
    Node2.G.readwrite Node2.G.t ->
    (Node2.scope_kind, Node2.misc) Node2.t ->
    string ->
    (a, b) Node2.t ->
    unit =
   fun g scope name node ->
    let (Node2.Scope tbl) = scope.Node2.kind in
    let { name = _; node = AnyNode symbol; is_const = _; is_forward_ref = _; idx = _ } =
        Symbol_table.find_symbol tbl name |> Core.Option.value_exn
    in
    let symbol =
        match symbol.kind with
        | Scope old_tbl -> (
            let { name = _; node = AnyNode tmp; is_const = _; is_forward_ref = _; idx = _ } =
                Symbol_table.find_symbol old_tbl name |> Core.Option.value_exn
            in
            let symbol = Node2.unpack_exn symbol (Scope old_tbl) in
            let (AnyCtrl ctrl) = get_ctrl g symbol in
            let (AnyNode tmp_ctrl) = Node2.G.get_ctrl_exn g tmp in
            match tmp.kind with
            | Data Phi when Node2.equal tmp_ctrl ctrl -> Node2.AnyNode symbol
            | Data _ ->
                let (AnyNode sym) = get g symbol name in
                let phi =
                    Phi_node.create_data_no_backedge g node.loc ctrl (Node2.as_data_exn sym)
                in
                assign g symbol name phi;
                AnyNode phi
            | Mem _ ->
                let (AnyNode sym) = get g symbol name in
                let phi = Phi_node.create_mem_no_backedge g node.loc ctrl (Node2.as_mem_exn sym) in
                assign g symbol name phi;
                AnyNode phi
            | _ -> assert false)
        | _ -> AnyNode symbol
    in
    Symbol_table.reassign_symbol tbl name (AnyNode node);
    let (AnyNode symbol_node) = symbol in
    (match (symbol_node.min_typ, node.min_typ) with
    | Some _, None -> node.min_typ <- symbol_node.min_typ
    | _ -> ());

    let { Node2.vars } = Node2.G.get_dependencies_exn g scope in
    let idx =
        List.findi vars ~f:(fun _ o ->
            match o with
            | None -> false
            | Some (AnyNode x) -> Node2.equal x symbol_node)
    in
    (match idx with
    | None -> Node2.G.set_node_inputs g scope { Node2.vars = Some (AnyNode node) :: vars }
    | Some (idx, _) ->
        let vars =
            List.mapi vars ~f:(fun i n ->
                if i = idx then
                  Some (Node2.AnyNode node)
                else
                  n)
        in
        Node2.G.set_node_inputs g scope { Node2.vars });
    match symbol_node.kind with
    | ForwardRef fref_name when String.equal fref_name name ->
        (* update uses of the forward ref to point to the actual thing *)
        (* TODO: this is kind of awful having to iterate through the entire
               symbol table to update names that point to the forward ref *)
        let is_in_symbol_table = Option.is_some idx in
        if is_in_symbol_table then
          Symbol_table.iter tbl (fun ~name ~symbol:other ~depth:_ ->
              match other with
              | Some other ->
                  let (AnyNode other_node) = other.node in
                  if Node2.equal symbol_node other_node then
                    Symbol_table.reassign_symbol tbl name (AnyNode node)
              | None -> ());
        Node2.G.replace_node_with g symbol_node node
    | _ -> ()

and get g scope name : Node2.any =
    let (Node2.Scope tbl) = scope.Node2.kind in
    let symbol = Symbol_table.find_symbol tbl name in
    match symbol with
    | None ->
        let fref = Node2.create_forward_ref name in
        define g scope name fref false;
        AnyNode fref
    | Some { name = _; node = AnyNode symbol; is_const = _; is_forward_ref = _; idx = _ } -> (
        match symbol.kind with
        | Scope old_tbl ->
            let { name = _; node = AnyNode tmp; is_const = _; is_forward_ref = _; idx = _ } =
                Symbol_table.find_symbol old_tbl name |> Core.Option.value_exn
            in
            let old_scope = Node2.unpack_exn symbol (Scope old_tbl) in
            let (AnyCtrl ctrl) = get_ctrl g old_scope in
            let (AnyNode tmp_ctrl) = Node2.G.get_ctrl_exn g tmp in
            let new_symbol =
                match tmp.kind with
                | Data Phi when Node2.equal tmp_ctrl ctrl -> Node2.AnyNode tmp
                | Data _ ->
                    let (AnyNode s) = get g old_scope name in
                    let s = Node2.as_data_exn s in
                    let phi = Phi_node.create_data_no_backedge g s.loc ctrl s in
                    assign g old_scope name phi;
                    AnyNode phi
                | Mem _ ->
                    let (AnyNode s) = get g old_scope name in
                    let s = Node2.as_mem_exn s in
                    let phi = Phi_node.create_mem_no_backedge g s.loc ctrl s in
                    assign g old_scope name phi;
                    AnyNode phi
                | _ -> assert false
            in
            let (AnyNode symbol_node) = new_symbol in
            assign g scope name symbol_node;
            new_symbol
        | _ -> Node2.AnyNode symbol)

and get_ctrl g scope : Node2.any_ctrl =
    let (AnyNode n) = get g scope ctrl_identifier in
    Node2.AnyCtrl (Node2.as_ctrl_exn n)

let set_ctrl g n ctrl =
    try assign g n ctrl_identifier ctrl with
    | _ -> define g n ctrl_identifier ctrl false

let get_mem g scope : Node2.any_mem =
    let (AnyNode n) = get g scope mem_identifier in
    Node2.AnyMem (Node2.as_mem_exn n)

let set_mem g n mem =
    try assign g n mem_identifier mem with
    | _ -> define g n mem_identifier mem false

let push scope =
    let (Scope tbl) = scope.Node2.kind in
    scope.kind <- Scope (Symbol_table.push tbl)

let pop g scope =
    let (Scope tbl) = scope.Node2.kind in
    let parent_table = Symbol_table.pop tbl in
    Symbol_table.iter_current_depth tbl (fun ~name ~symbol ->
        let { name = _; node = AnyNode symbol_node; is_const = _; is_forward_ref = _; idx = _ } =
            symbol
        in
        match symbol_node.kind with
        | ForwardRef _ ->
            (* bubble up forward ref to parent scope *)
            Symbol_table.add_symbol parent_table name symbol
        | _ ->
            let { Node2.vars } = Node2.G.get_dependencies_exn g scope in
            Node2.G.set_node_inputs g scope
              {
                vars =
                  List.filter vars ~f:(function
                    | None -> true
                    | Some (AnyNode n) -> not (Node2.equal n symbol_node));
              });
    scope.kind <- Scope parent_table

let dup g scope =
    let (Scope tbl) = scope.Node2.kind in
    let scope_dup = Node2.create_scope () in
    Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
        match symbol with
        | None -> push scope_dup
        | Some { name = _; node = AnyNode node; is_const; is_forward_ref = _; idx = _ } ->
            define g scope_dup name node is_const);
    scope_dup

(* Set symbols to point to the current symbol node itself (not the duped one), that way we can easily detect the symbols when they are first used in the loop and create phi nodes for them *)
let dup_loop g scope =
    let (Scope tbl) = scope.Node2.kind in
    let scope_dup = Node2.create_scope () in
    Symbol_table.iter tbl (fun ~name ~symbol ~depth:_ ->
        match symbol with
        | None -> push scope_dup
        | Some { name = _; node = AnyNode node; is_const; is_forward_ref = _; idx = _ } ->
            if String.equal name ctrl_identifier then
              let (AnyCtrl ctrl) = get_ctrl g scope in
              define g scope_dup name ctrl is_const
            else if is_const then
              define g scope_dup name node is_const
            else
              define g scope_dup name scope is_const);
    scope_dup

let merge ?parent_fun g loc ~this ~other =
    (* Same as `get` but can't have it assign the name to the phi node in the current table as we can't mutate the tabl while iterating *)
    let get_no_insert g tbl name =
        let {
          name = _;
          node = Node2.AnyNode symbol_node;
          is_const = _;
          is_forward_ref = _;
          idx = _;
        } =
            Symbol_table.find_symbol tbl name |> Core.Option.value_exn
        in
        match symbol_node.kind with
        | Scope _ -> (
            (* 
               This happens if the if statement is in a loop's body.
               At this point the phi node must exist as one of the branches has changed (used) this symbol.
               What can happen is that if only the "else" branch modifies a symbol, the phi node gets created in the "else" scope and the original scope (the loop's original scope, so not the loop body). Because of this we don't want to create a new phi, just get it from the original scope *)
            let (AnyNode node) = get g symbol_node name in
            match node.kind with
            | Scope _ -> assert false (* This shouldn't happen i think *)
            | _ -> Node2.AnyNode node)
        | _ -> AnyNode symbol_node
    in
    let (Scope this_tbl) = this.Node2.kind in
    let (Scope other_tbl) = other.Node2.kind in
    let region = Region_node.create ?parent_fun g loc [ get_ctrl g this; get_ctrl g other ] in
    let old_ctrl = get_ctrl g this in
    let diff_fn ~name ~this:v_this ~other:_ =
        let (Node2.AnyNode v_this_node) = v_this.Variable.node in
        let (AnyCtrl old_ctrl) = old_ctrl in
        if not (Node2.equal v_this_node old_ctrl) then (
          let (AnyNode this_n) = get_no_insert g this_tbl name in
          let (AnyNode other_n) = get_no_insert g other_tbl name in
          let phi =
              match (this_n.kind, other_n.kind) with
              | Data _, Data _ ->
                  let this_n = Node2.as_data_exn this_n in
                  let other_n = Node2.as_data_exn other_n in
                  let p =
                      Phi_node.create_data ?parent_fun g loc region
                        [ Node2.AnyData this_n; AnyData other_n ]
                  in
                  Node2.AnyNode p
              | Mem _, Mem _ ->
                  let this_n = Node2.as_mem_exn this_n in
                  let other_n = Node2.as_mem_exn other_n in
                  let p =
                      Phi_node.create_mem ?parent_fun g loc region
                        [ Node2.AnyMem this_n; AnyMem other_n ]
                  in
                  Node2.AnyNode p
              | _, _ -> assert false
          in
          let { Node2.vars } = Node2.G.get_dependencies_exn g this in
          let vars =
              List.mapi vars ~f:(fun i n ->
                  match n with
                  | None -> None
                  | Some (AnyNode n) ->
                      if Node2.equal v_this_node n then
                        Some phi
                      else
                        Some (AnyNode n))
          in
          Node2.G.set_node_inputs g this { Node2.vars };
          { v_this with node = phi })
        else
          { v_this with node = AnyNode old_ctrl }
    in
    Symbol_table.merge this_tbl other_tbl diff_fn (fun v v' ->
        let (AnyNode n) = v.node in
        let (AnyNode n') = v'.node in
        Node2.equal n n');
    set_ctrl g this region;
    Node2.G.remove_node g other

let merge_loop ?parent_fun g ~this ~body ~exit =
    let (Scope this_tbl) = this.Node2.kind in
    let (Scope body_tbl) = body.Node2.kind in
    let (Scope exit_tbl) = exit.Node2.kind in
    let phis = ref [] in
    Symbol_table.iter body_tbl (fun ~name ~symbol ~depth:_ ->
        match symbol with
        | None -> ()
        | Some symbol -> (
            let (AnyNode symbol_node) = symbol.node in
            (* Set the second input of the phi node *)
            if
              (not (String.equal name ctrl_identifier))
              && (not symbol.is_const)
              && not (Node2.equal symbol_node this)
            then
              let v_this = Symbol_table.find_symbol this_tbl name |> Core.Option.value_exn in
              let v_body = Symbol_table.find_symbol body_tbl name |> Core.Option.value_exn in

              let (AnyNode this_node) = v_this.node in
              let (AnyNode body_node) = v_body.node in
              match this_node.kind with
              | Data Phi ->
                  let phi = Node2.unpack_exn this_node (Data Phi) in
                  if Node2.equal this_node body_node then (
                    let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g phi in
                    let value = List.nth_exn phi_inputs 1 |> Option.value_exn in
                    Node2.G.replace_node_with g v_this.node value;
                    (* symbols get assigned from exit table to this table later *)
                    let (AnyData value) = value in
                    Symbol_table.reassign_symbol exit_tbl name (AnyNode value))
                  else (
                    phis := Node2.AnyNode phi :: !phis;
                    let (AnyNode value) = v_body.node in
                    let value = Node2.as_data_exn value in
                    Phi_node.add_backedge_input_data g phi value)
              | Mem Phi ->
                  let phi = Node2.unpack_exn this_node (Mem Phi) in
                  if Node2.equal this_node body_node then (
                    let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g phi in
                    let value = List.nth_exn phi_inputs 1 |> Option.value_exn in
                    Node2.G.replace_node_with g v_this.node value;
                    (* symbols get assigned from exit table to this table later *)
                    let (AnyMem value) = value in
                    Symbol_table.reassign_symbol exit_tbl name (AnyNode value))
                  else (
                    phis := AnyNode phi :: !phis;
                    let (AnyNode value) = v_body.node in
                    let value = Node2.as_mem_exn value in
                    Phi_node.add_backedge_input_mem g phi value)
              | _ -> assert false));

    (* TODO: we put phis to bottom type for now. we'll have constant propagation that calculates better type later in SCCP *)
    Core.List.iter !phis ~f:(fun (AnyNode n) ->
        n.typ <- ALL;
        n.parent_fun <- parent_fun);
    Symbol_table.iter exit_tbl (fun ~name ~symbol ~depth:_ ->
        match symbol with
        | None -> ()
        | Some symbol ->
            let (AnyNode symbol_node) = symbol.node in
            if
              (not (String.equal name ctrl_identifier))
              && (not symbol.is_const)
              && not (Node2.equal symbol_node this)
            then
              Symbol_table.reassign_symbol this_tbl name symbol.node);
    let (AnyCtrl ctrl) = get_ctrl g exit in
    set_ctrl g this ctrl;
    Node2.G.remove_node g body;
    Node2.G.remove_node g exit
