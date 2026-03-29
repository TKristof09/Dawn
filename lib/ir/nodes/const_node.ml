open Core

let create_zint g loc ?parent_fun i =
    let n = Node.create_data ?parent_fun loc (Types.make_int_const i) Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n

let create_int g loc ?parent_fun i =
    let n = Node.create_data ?parent_fun loc (Types.make_int_const (Z.of_int i)) Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n

let create_bool g loc ?parent_fun b =
    let n = Node.create_data ?parent_fun loc (Types.Bool (Value b)) Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n

let create_fun_ptr g loc ?parent_fun (fun_node : Node.t) idx =
    let signature = Fun_node.get_signature fun_node in
    let typ =
        match signature with
        | FunPtr (Value ptr) -> Types.make_fun_ptr ~idx ptr.params ptr.ret
        | FunPtr _ -> failwith "Should be fine but idk"
        | _ -> failwithf "Expected function ptr but got %s" (Types.show signature) ()
    in
    let n = Node.create_data ?parent_fun loc typ Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n

let create_string g loc ?parent_fun s =
    let arr_typ = Types.make_string s in
    let n = Node.create_data ?parent_fun loc arr_typ Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n

let create_from_type g loc ?parent_fun typ =
    assert (Types.is_constant typ);
    let n = Node.create_data ?parent_fun loc typ Constant in
    Graph.add_dependencies g n [ Some (Graph.get_start g) ];
    Graph.finalize_node g n
