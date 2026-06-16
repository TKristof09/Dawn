open Core

let create_new g loc ?parent_fun ~ctrl ~mem ~size typ =
    let node_type = Types.Tuple (Value [ Memory; typ ]) in
    let n = Node2.create_mem ?parent_fun loc node_type New in
    Node2.G.add_node g n { Node2.mem = Some (AnyMem mem); size = Some (AnyData size) };
    Node2.G.set_ctrl g n ctrl;
    n

let create_store g loc ?parent_fun ~mem ~ptr field_name ~value =
    let n = Node2.create_mem ?parent_fun loc Memory (Store field_name) in
    Node2.G.add_node g n
      { Node2.mem = Some (AnyMem mem); ptr = Some (AnyData ptr); value = Some (AnyData value) };
    n

let create_load g loc ?parent_fun ~mem ~ptr field_name field_typ =
    let n = Node2.create_data ?parent_fun loc field_typ (Load field_name) in
    Node2.G.add_node g n { Node2.mem = Some (AnyMem mem); ptr = Some (AnyData ptr) };
    n

let create_addr_of g loc ?parent_fun place =
    let n = Node2.create_data ?parent_fun loc (Ptr place.Node2.typ) AddrOf in
    Node2.G.add_node g n { Node2.place = Some (AnyData place); offset = None };
    n

let create_addr_of_field g loc ?parent_fun place ?index field_name =
    let t = Types.get_field_type place.Node2.typ field_name |> Option.value ~default:ALL in
    (* index only for array field type *)
    let t =
        match index with
        | None -> t
        | Some _ -> (
            match t with
            | Array (Value { element_type; values = _ })
            | ConstArray (Value { element_type; values = _ }) ->
                element_type
            | _ ->
                (* index can only be with array types *)
                assert false)
    in

    let n = Node2.create_data ?parent_fun loc (Ptr t) (AddrOfField field_name) in
    Node2.G.add_node g n
      {
        Node2.place = Some (AnyData place);
        offset = Option.map index ~f:(fun index -> Node2.AnyData index);
      };
    n

let create_deref g loc ?parent_fun ~mem ptr =
    let t =
        match ptr.Node2.typ with
        | Ptr p -> p
        | _ -> assert false
    in
    let n = Node2.create_data ?parent_fun loc t Deref in
    Node2.G.add_node g n { Node2.mem = Some (AnyMem mem); ptr = Some (AnyData ptr) };
    n

let create_copy ?parent_fun g loc ~mem ~src ~dst =
    let n = Node2.create_mem ?parent_fun loc Memory Copy in
    Node2.G.add_node g n
      { Node2.mem = Some (AnyMem mem); src = Some (AnyData src); dst = Some (AnyData dst) };
    n
