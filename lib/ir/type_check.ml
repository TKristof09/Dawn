open Core

let expect_types (loc : Ast.loc) ~expected ~actual =
    match List.zip expected actual with
    | Unequal_lengths -> assert false
    | Ok l ->
        let errors =
            List.filter l ~f:(fun (expected, actual) -> not (Types.is_a actual expected))
            |> List.map ~f:(fun (expected, actual) ->
                let msg =
                    Printf.sprintf "Expected %s, got %s" (Types.human_readable expected)
                      (Types.human_readable actual)
                in
                (loc, msg))
        in
        if List.is_empty errors then
          None
        else
          Some errors

let as_binop : type a b. (a, b) Node2.t -> (Node2.binop, Node2.data) Node2.t =
   fun n ->
    match n.kind with
    | Data Add -> n
    | Data Sub -> n
    | Data Div -> n
    | Data Mul -> n
    | Data Eq -> n
    | Data NEq -> n
    | Data Lt -> n
    | Data LEq -> n
    | Data Gt -> n
    | Data GEq -> n
    | Data BAnd -> n
    | Data BOr -> n
    | _ -> assert false

let do_data_node : type a.
    Node2.G.readonly Node2.G.t -> (a, Node2.data) Node2.t -> (Ast.loc * string) list option =
   fun g n ->
    match n.kind with
    | Data Add
    | Data Sub
    | Data Mul
    | Data Div ->
        let n = as_binop n in
        let { Node2.lhs; rhs } = Node2.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let expected = [ Types.i64; Types.i64 ] in
        let actual = [ lhs.typ; rhs.typ ] in
        expect_types n.loc ~expected ~actual
    | Data Lsh
    | Data Rsh
    | Data BAnd
    | Data BOr
    | Data Eq
    | Data NEq
    | Data Lt
    | Data LEq
    | Data Gt
    | Data GEq ->
        let n = as_binop n in
        let { Node2.lhs; rhs } = Node2.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let expected = [ Types.i64; Types.i64 ] in
        let actual = [ lhs.typ; rhs.typ ] in
        expect_types n.loc ~expected ~actual
    | Data Constant -> None
    | Data (Proj _) -> None
    | Data Phi ->
        (* FIXME: This feels kind of useless since phi.typ is always the meet of its alive inputs so this expected_types should never find errors since every input isa phi.typ by definition *)
        let (AnyNode region) = Node2.G.get_ctrl_exn g n in
        let ctrl_inputs =
            match region.kind with
            | Ctrl Region ->
                let { Node2.ctrl_inputs } = Node2.G.get_dependencies_exn g region in
                ctrl_inputs
            | Ctrl Loop ->
                let { Node2.entry; backedge } = Node2.G.get_dependencies_exn g region in
                [ entry; backedge ]
            | _ -> assert false
        in
        let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
        let inputs =
            List.zip_exn ctrl_inputs phi_inputs
            |> List.filter_map ~f:(fun (c, d) ->
                match (c, d) with
                | Some (AnyCtrl c), Some (AnyData d) -> (
                    match c.typ with
                    | DeadControl -> None
                    | _ -> Some d.typ)
                | _ -> None)
        in
        let expected = List.init (List.length inputs) ~f:(Fun.const n.typ) in
        expect_types n.loc ~expected ~actual:inputs
    | Data (Param _) ->
        (* Error reporting happens at FunctionCall nodes for better error location tracking *)
        None
    | Data (External _) -> None
    | Data Cast ->
        (* TODO: probably should check *something* *)
        None
    | ForwardRef name -> Some [ (n.loc, Printf.sprintf "Symbol %s not found" name) ]

let do_ctrl_node : type a.
    Node2.G.readonly Node2.G.t -> (a, Node2.ctrl) Node2.t -> (Ast.loc * string) list option =
   fun g n ->
    match n.kind with
    | Ctrl Start -> None
    | Ctrl Stop -> None
    | Ctrl (Proj _) -> None
    | Ctrl If ->
        let { Node2.input } = Node2.G.get_dependencies_exn g n in
        let (AnyData input) = Option.value_exn input in
        (* TODO bool type *)
        if Types.is_a input.typ (Bool All) then
          None
        else
          Some [ (n.loc, Printf.sprintf "Expected bool got %s" (Types.human_readable input.typ)) ]
    | Ctrl Region -> None
    | Ctrl Loop -> None
    | Ctrl (Function { ret; signature; idx = _ }) ->
        (* It's easier to check return type here because we have access to both signature and return node *)
        let expected_ret_type =
            match signature with
            | FunPtr (Value { params = _; ret; fun_indices = _ }) -> ret
            | _ -> assert false
        in
        let actual_ret_type =
            match ret.typ with
            | Tuple (Value [ _; _; ret_type ]) -> ret_type
            | _ -> assert false
        in
        expect_types ret.loc ~expected:[ expected_ret_type ] ~actual:[ actual_ret_type ]
    | Ctrl Return ->
        (* return type checked in the Function node check because that has easier access to expected signature *)
        None
    | Ctrl FunctionCall ->
        let { Node2.fun_ptr; mem; args } = Node2.G.get_dependencies_exn g n in
        let (AnyData fun_ptr) = Option.value_exn fun_ptr in
        let expected =
            match fun_ptr.typ with
            | FunPtr (Value { params; ret = _; fun_indices = _ }) -> params
            | _ -> assert false
        in
        let actual_args = args |> List.filter_opt |> List.map ~f:(fun (AnyData n) -> n.typ) in
        if List.length expected <> List.length actual_args then
          Some
            [
              ( n.loc,
                Printf.sprintf "Invalid number of arguments, got %d expected %d"
                  (List.length actual_args) (List.length expected) );
            ]
        else
          expect_types n.loc ~expected ~actual:actual_args
    | Ctrl FunctionCallEnd -> None

let do_mem_node : type a.
    Node2.G.readonly Node2.G.t -> (a, Node2.mem) Node2.t -> (Ast.loc * string) list option =
   fun g n ->
    match n.kind with
    | Mem New ->
        let { Node2.mem; size } = Node2.G.get_dependencies_exn g n in
        let (AnyData size) = Option.value_exn size in
        if Types.is_a size.typ Types.i64 then
          None
        else
          Some [ (n.loc, Printf.sprintf "Expected integer got %s" (Types.human_readable size.typ)) ]
    | Mem (Store name) -> (
        let { Node2.mem; ptr; value } = Node2.G.get_dependencies_exn g n in
        let (AnyMem ptr) = Option.value_exn ptr in
        let (AnyData value) = Option.value_exn value in

        match ptr.typ with
        | Ptr p ->
            let expected = [ p ] in
            expect_types n.loc ~expected ~actual:[ value.typ ]
        | _ ->
            Some
              [
                ( n.loc,
                  Printf.sprintf "Memory store expected pointer got %s"
                    (Types.human_readable ptr.typ) );
              ])
    | Mem (Load name) ->
        let { Node2.mem; ptr } : Node2.load = Node2.G.get_dependencies_exn g n in
        let (AnyMem ptr) = Option.value_exn ptr in
        expect_types n.loc ~expected:[ Types.Ptr ALL ] ~actual:[ ptr.typ ]
    | Mem AddrOf -> None
    | Mem (AddrOfField field) ->
        let { Node2.place; offset } = Node2.G.get_dependencies_exn g n in
        let (AnyData place) = Option.value_exn place in
        let type_errors = expect_types n.loc ~expected:[ Struct All ] ~actual:[ place.typ ] in
        let field_type = Types.get_field_type place.typ field in
        if Option.is_none type_errors then
          match
            field_type
          with
          | None ->
              Some
                [
                  ( n.loc,
                    Printf.sprintf "Field %s not part of type %s" field
                      (Types.human_readable place.typ) );
                ]
          | Some t ->
              if Option.is_some offset && not (Types.is_a t (Array All)) then
                Some [ (n.loc, Printf.sprintf "Field %s is not indexable" field) ]
              else
                None
        else
          type_errors
    | Mem Deref ->
        let { Node2.mem; ptr } = Node2.G.get_dependencies_exn g n in
        let (AnyMem ptr) = Option.value_exn ptr in
        expect_types n.loc ~expected:[ Types.Ptr ALL ] ~actual:[ ptr.typ ]
    | Mem Copy ->
        let { Node2.mem; src; dst } = Node2.G.get_dependencies_exn g n in
        let (AnyMem src) = Option.value_exn src in
        let (AnyMem dst) = Option.value_exn dst in
        let type_errors =
            expect_types n.loc ~expected:[ Types.Ptr ALL; Types.Ptr ALL ]
              ~actual:[ src.typ; dst.typ ]
        in
        if Option.is_none type_errors then
          let src_size =
              match src.typ with
              | Ptr p -> Types.get_size p
              | _ -> assert false
          in
          let dst_size =
              match dst.typ with
              | Ptr p -> Types.get_size p
              | _ -> assert false
          in
          if src_size <> dst_size then
            Some
              [
                ( n.loc,
                  Printf.sprintf
                    "Two operands of mem copy are of different sizes (src: %d, dst: %d)" src_size
                    dst_size );
              ]
          else
            None
        else
          type_errors
    | Mem Phi ->
        (* These arent real things *)
        None
    | Mem Param ->
        (* These arent real things *)
        None

let type_check_node : type a b.
    Node2.G.readonly Node2.G.t -> (a, b) Node2.t -> (Ast.loc * string) list option =
   fun g n ->
    match n.kind with
    | Data d -> do_data_node g n
    | Ctrl c -> do_ctrl_node g n
    | Scope _ -> None
    | Mem m -> do_mem_node g n
    | ForwardRef name -> Some [ (n.loc, Printf.sprintf "Undefined symbol %s" name) ]

let run g =
    Node2.G.fold g ~init:[] ~f:(fun errors (AnyNode n) ->
        match type_check_node g n with
        | None -> errors
        | Some errs -> errs @ errors)
    |> List.sort ~compare:(fun (loc, _) (loc', _) -> Ast.compare_loc loc loc')
    |> List.map ~f:(fun (loc, msg) -> Printf.sprintf "%s:%d: %s" loc.filename loc.line msg)
