open Core

let common g loc parent_fun typ =
    let n = Node.create_data ?parent_fun loc typ Constant in
    Node.G.add_node g n ();
    let (AnyNode start) = Node.G.get_start g in
    Node.G.set_ctrl g n start;
    n

let create_zint g loc ?parent_fun i = common g loc parent_fun (Types.make_int_const i)
let create_int g loc ?parent_fun i = common g loc parent_fun (Types.make_int_const (Z.of_int i))
let create_bool g loc ?parent_fun b = common g loc parent_fun (Types.Bool (Value b))

let create_fun_ptr g loc ?parent_fun fun_node idx =
    let signature = Fun_node.get_signature fun_node in
    let typ =
        match signature with
        | FunPtr (Value ptr) -> Types.make_fun_ptr ~idx ptr.params ptr.ret
        | FunPtr _ -> failwith "Should be fine but idk"
        | _ -> failwithf "Expected function ptr but got %s" (Types.show signature) ()
    in
    common g loc parent_fun typ

let create_string g loc ?parent_fun s =
    let arr_typ = Types.make_string s in
    common g loc parent_fun arr_typ

let create_from_type g loc ?parent_fun typ =
    assert (Types.is_constant typ);
    common g loc parent_fun typ
