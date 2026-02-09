open Core

let create_int g loc i =
    let n =
        let n = Node.create_data loc (Types.Integer (Value i)) Constant in
        Graph.add_dependencies g n [ Some (Graph.get_start g) ];
        n
    in
    Graph.finalize_node g n

let create_fun_ptr g loc (fun_node : Node.t) idx =
    let signature = Fun_node.get_signature fun_node in
    let typ =
        match signature with
        | FunPtr (Value ptr) -> Types.make_fun_ptr ~idx ptr.params ptr.ret
        | FunPtr _ -> failwith "Should be fine but idk"
        | _ -> failwithf "Expected function ptr but got %s" (Types.show_node_type signature) ()
    in
    let n = Node.create_data loc typ Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n

let create_string g loc s =
    let arr_typ = Types.make_string s in
    let n = Node.create_data loc (Ptr arr_typ) Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n
