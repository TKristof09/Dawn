open Core

let create g ret_typ =
    let return_value = Node.create_data ret_typ Phi in
    let return_region = Node.create_ctrl Control Region in
    Graph.add_dependencies g return_region [ None ];
    let ret_node_typ : Types.node_type =
        match return_value.typ with
        | Tuple (Value t) -> Tuple (Value (Control :: t))
        | v -> Tuple (Value [ Control; v ])
    in
    let ret_node = Node.create_ctrl ret_node_typ Return in
    Graph.add_dependencies g ret_node [ Some return_region; Some return_value ];
    let fun_node = Node.create_ctrl Control (Function ret_node) in
    let start = Graph.get_start g in
    Graph.add_dependencies g fun_node [ None; Some start ];
    (fun_node, ret_node)

let create_param g fun_node param_type i =
    let n = Node.create_data param_type (Param i) in
    Graph.add_dependencies g n [ Some fun_node ];
    n

let add_call g ctrl fun_node args =
    let call = Node.create_ctrl Control FunctionCall in
    Graph.add_dependencies g call (Some ctrl :: List.map args ~f:Option.some);
    Graph.add_dependencies g fun_node [ Some call ];
    let param_nodes =
        Graph.get_dependants g fun_node
        |> List.filter ~f:(fun n ->
               match n.kind with
               | Data (Param _) -> true
               | _ -> false)
    in
    List.zip_exn param_nodes args
    |> List.iter ~f:(fun (param, arg) -> Graph.add_dependencies g param [ Some arg ]);
    let ret_node =
        match fun_node.kind with
        | Ctrl (Function ret_node) -> ret_node
        | _ -> assert false
    in
    let call_end = Node.create_ctrl ret_node.typ FunctionCallEnd in
    Graph.add_dependencies g call_end [ Some call; Some ret_node ];
    call_end

let add_return g ret_node ~ctrl ~val_n =
    let region = Graph.get_dependency g ret_node 0 |> Option.value_exn in
    let phi = Graph.get_dependency g ret_node 1 |> Option.value_exn in
    Graph.add_dependencies g phi [ Some val_n ];
    Phi_node.compute_type g phi;
    Graph.add_dependencies g region [ Some ctrl ]
