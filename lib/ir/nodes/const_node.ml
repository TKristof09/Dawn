open Core

let create_int g i =
    let n =
        let n = Node.create_data (Types.Integer (Value i)) Constant in
        Graph.add_dependencies g n [ Some (Graph.get_start g) ];
        n
    in
    Graph.finalize_node g n

let create_fun_ptr g (fun_node : Node.t) idx =
    let signature = Fun_node.get_signature fun_node in
    let typ =
        match signature with
        | FunPtr (Value ptr) ->
            Types.FunPtr (Value { ptr with fun_indices = `Include (Int.Set.singleton idx) })
        | FunPtr _ -> failwith "Should be fine but idk"
        | _ -> failwithf "Expected function ptr but got %s" (Types.show_node_type signature) ()
    in
    let n = Node.create_data typ Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n
