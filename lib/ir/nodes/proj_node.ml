open Core

let create_data g loc ?parent_fun n i =
    match n.Node.typ with
    | Tuple (Value l) -> (
        match List.nth_exn l i with
        | Types.Control -> assert false
        | t ->
            let proj = Node.create_data ?parent_fun loc t (Proj i) in
            Node.G.add_node g proj { Node.input = Some (AnyNode n) };
            proj)
    | Tuple _ -> failwith "idk if this should be possible"
    | _ -> failwith "Proj node needs an input with tuple type"

let create_ctrl g loc ?parent_fun n i =
    match n.Node.typ with
    | Tuple (Value l) -> (
        match List.nth_exn l i with
        | Types.Control ->
            let proj = Node.create_ctrl ?parent_fun loc Control (Proj i) in
            Node.G.add_node g proj { Node.input = Some (AnyNode n) };
            proj
        | t -> assert false)
    | Tuple _ -> failwith "idk if this should be possible"
    | _ -> failwith "Proj node needs an input with tuple type"

let create_mem g loc ?parent_fun n i =
    match n.Node.typ with
    | Tuple (Value l) -> (
        match List.nth_exn l i with
        | Types.Memory ->
            let proj = Node.create_mem ?parent_fun loc Memory (Proj i) in
            Node.G.add_node g proj { Node.input = Some (AnyNode n) };
            proj
        | t -> assert false)
    | Tuple _ -> failwith "idk if this should be possible"
    | _ -> failwith "Proj node needs an input with tuple type"

let compute_type : type a b.
    Node.G.readonly Node.G.t ->
    (Node.any Node.unary, b) Node.t ->
    (new_type:Types.t * extra_deps:Node.any list) =
   fun g n ->
    let { Node.input } = Node.G.get_dependencies_exn g n in
    let (Node.AnyNode input) = Option.value_exn input in
    let i =
        match n.kind with
        | Data (Proj i) -> i
        | Ctrl (Proj i) -> i
        | Mem (Proj i) -> i
    in
    let typ =
        match input.typ with
        | Tuple (Value l) -> List.nth_exn l i
        | ANY -> ANY
        | _ -> Types.ALL
    in
    let new_type = typ in
    (~new_type, ~extra_deps:[])
