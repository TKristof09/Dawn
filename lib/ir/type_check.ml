open Core

let expect_types (loc : Ast.loc) ~expected ~inputs =
    match List.zip expected inputs with
    | Unequal_lengths -> assert false
    | Ok l ->
        let errors =
            List.filter l ~f:(fun (expected, actual) -> not (Types.is_a actual expected))
            |> List.map ~f:(fun (expected, actual) ->
                Printf.sprintf "%s:%d: Expected %s, got %s" loc.filename loc.line
                  (Types.human_readable expected) (Types.human_readable actual))
        in
        if List.is_empty errors then
          None
        else
          Some errors

let do_data_node g (n : Node.t) (k : Node.data_kind) =
    let inputs =
        Graph.get_dependencies g n |> List.tl_exn |> List.filter_opt |> List.map ~f:(fun n -> n.typ)
    in
    match k with
    | Add
    | Sub
    | Mul
    | Div
    | Lsh
    | Rsh
    | BAnd
    | BOr
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq ->
        let expected = [ Types.Integer All; Integer All ] in
        expect_types n.loc ~expected ~inputs
    | Constant -> None
    | Proj _ -> None
    | Phi ->
        (* FIXME: This feels kind of useless since phi.typ is always the meet of its alive inputs so this expected_types should never find errors since every input isa phi.typ by definition *)
        let in_control =
            Graph.get_dependency g n 0
            |> Option.value_exn
            |> Graph.get_dependencies g
            |> List.tl_exn
        in
        let in_data = Graph.get_dependencies g n |> List.tl_exn in
        let inputs =
            List.zip_exn in_control in_data
            |> List.filter_map ~f:(fun (c, d) ->
                match (c, d) with
                | Some c, Some d -> (
                    match c.Node.typ with
                    | DeadControl -> None
                    | _ -> Some d.typ)
                | _ -> None)
        in
        let expected = List.init (List.length inputs) ~f:(Fun.const n.typ) in
        expect_types n.loc ~expected ~inputs
    | Param _ ->
        (* Error reporting happens at FunctionCall nodes for better error location tracking *)
        None
    | External _ -> None

let do_ctrl_node g (n : Node.t) (k : Node.ctrl_kind) =
    match k with
    | Start -> None
    | Stop -> None
    | Proj _ -> None
    | If ->
        let cond = Graph.get_dependency g n 1 |> Option.value_exn in
        (* TODO bool type *)
        if Types.is_a cond.typ (Integer All) then
          None
        else
          Some
            [
              Printf.sprintf "%s:%d: Expected bool got %s" n.loc.filename n.loc.line
                (Types.human_readable cond.typ);
            ]
    | Region -> None
    | Loop -> None
    | Function _ -> None
    | Return -> None
    | FunctionCall ->
        let fun_ptr_node = Graph.get_dependency g n 1 |> Option.value_exn in
        let expected =
            match fun_ptr_node.typ with
            | FunPtr (Value { params; ret = _; fun_indices = _ }) -> params
            | _ -> assert false
        in
        let actual_args =
            Graph.get_dependencies g n
            |> List.tl_exn (* drop control *)
            |> List.tl_exn (* drop fun ptr *)
            |> List.filter_opt
            |> List.map ~f:(fun n -> n.typ)
        in
        if List.length expected <> List.length actual_args then
          Some
            [
              Printf.sprintf "%s:%d: Invalid number of arguments, got %d expected %d" n.loc.filename
                n.loc.line (List.length actual_args) (List.length expected);
            ]
        else
          expect_types n.loc ~expected ~inputs:actual_args
    | FunctionCallEnd -> None

let do_mem_node g (n : Node.t) (k : Node.mem_kind) =
    match k with
    | New ->
        let size = Graph.get_dependency g n 2 |> Option.value_exn in
        if Types.is_a size.typ (Integer All) then
          None
        else
          Some
            [
              Printf.sprintf "%s:%d: Expected integer got %s" n.loc.filename n.loc.line
                (Types.human_readable size.typ);
            ]
    | Store name ->
        let ptr = Graph.get_dependency g n 2 |> Option.value_exn in
        let offset = Graph.get_dependency g n 3 |> Option.value_exn in
        let value = Graph.get_dependency g n 4 |> Option.value_exn in

        let pointed_to_type =
            match ptr.typ with
            | Ptr p -> p
            | _ -> assert false
        in
        let field_type = Types.get_field_type pointed_to_type name in
        let expected = [ field_type; Integer All ] in
        expect_types n.loc ~expected ~inputs:[ value.typ; offset.typ ]
    | Load _ -> (
        let ptr = Graph.get_dependency g n 2 |> Option.value_exn in
        let offset = Graph.get_dependency g n 3 |> Option.value_exn in
        let ptr_error =
            match ptr.typ with
            | Ptr _ -> []
            | _ ->
                [
                  Printf.sprintf "%s:%d: Expected pointer, got %s" n.loc.filename n.loc.line
                    (Types.human_readable ptr.typ);
                ]
        in
        match expect_types n.loc ~expected:[ Integer All ] ~inputs:[ offset.typ ] with
        | None -> if List.is_empty ptr_error then None else Some ptr_error
        | Some errors -> Some (ptr_error @ errors))

let type_check_node g (n : Node.t) =
    match n.kind with
    | Data d -> do_data_node g n d
    | Ctrl c -> do_ctrl_node g n c
    | Scope _ -> None
    | Mem m -> do_mem_node g n m

let run (g : (Node.t, 'b) Graph.t) =
    Graph.fold g ~init:[] ~f:(fun errors n ->
        match type_check_node g n with
        | None -> errors
        | Some errs -> errs @ errors)
