open Core

let create_data g loc ?parent_fun n i =
    match n.Node2.typ with
    | Tuple (Value l) -> (
        match List.nth_exn l i with
        | Types.Control -> assert false
        | t ->
            let proj = Node2.create_data ?parent_fun loc t (Proj i) in
            Node2.G.add_node g proj { Node2.input = Some (AnyData n) };
            proj)
    | Tuple _ -> failwith "idk if this should be possible"
    | _ -> failwith "Proj node needs an input with tuple type"

let create_ctrl g loc ?parent_fun n i =
    match n.Node2.typ with
    | Tuple (Value l) -> (
        match List.nth_exn l i with
        | Types.Control ->
            let proj = Node2.create_ctrl ?parent_fun loc Control (Proj i) in
            Node2.G.add_node g proj { Node2.input = Some (AnyCtrl n) };
            proj
        | t -> assert false)
    | Tuple _ -> failwith "idk if this should be possible"
    | _ -> failwith "Proj node needs an input with tuple type"

let compute_type : type a b.
    Node2.G.readonly Node2.G.t ->
    (a Node2.unary, b) Node2.t ->
    (new_type:Types.t * extra_deps:Node2.any list) =
   fun g n ->
    let new_type =
        match n.kind with
        | Ctrl (Proj i) ->
            let { Node2.input } = Node2.G.get_dependencies_exn g n in
            let (AnyCtrl input) = Option.value_exn input in
            let typ =
                match input.typ with
                | Tuple (Value l) -> List.nth_exn l i
                | ANY -> ANY
                | _ -> Types.ALL
            in
            typ
        | Data (Proj i) ->
            let { Node2.input } = Node2.G.get_dependencies_exn g n in
            let (AnyData input) = Option.value_exn input in
            let typ =
                match input.typ with
                | Tuple (Value l) -> List.nth_exn l i
                | ANY -> ANY
                | _ -> Types.ALL
            in
            typ
        | _ -> assert false
    in
    (~new_type, ~extra_deps:[])
