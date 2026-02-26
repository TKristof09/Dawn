open Core

type 'a sub_lattice =
    | Any
    | Value of 'a
    | All
[@@deriving sexp_of, equal]

let pp_sub_lattice pp_a fmt = function
    | Any -> Format.fprintf fmt "Any"
    | All -> Format.fprintf fmt "All"
    | Value v -> pp_a fmt v

let pp_fun_indices fmt = function
    | `Include set ->
        Format.fprintf fmt "`Include %a"
          (Format.pp_print_list Format.pp_print_int)
          (Set.to_list set)
    | `Exclude set ->
        Format.fprintf fmt "`Exclude %a"
          (Format.pp_print_list Format.pp_print_int)
          (Set.to_list set)

type fun_ptr = {
    params : t list;
    ret : t;
    (* cofinite sets to allow us to move up/down the lattice nicely without
       having to resort to having a finite number of functions. `Include
       means only the ids in the Int.Set.t, exclude means everything except
       the ids in the Int.Set.t *)
    fun_indices : [ `Include of Int.Set.t | `Exclude of Int.Set.t ]; [@printer pp_fun_indices]
  }

and struct_type = {
    name : string;
    fields : (string * t) list;
  }

and const_array = t list

and t =
    | ANY
    | Integer of int sub_lattice
    | Tuple of t list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of t
    | Struct of struct_type sub_lattice
    | ConstArray of const_array sub_lattice
    | Memory
    | Control
    | DeadControl
    | ALL
[@@deriving show { with_path = false }, sexp_of, equal]

let make_fun_ptr ?idx params ret =
    let is_valid_param_type t =
        match t with
        | Integer _
        | Ptr _ ->
            true
        | _ -> false
    in
    let is_valid_ret_type =
        match ret with
        | Integer _
        | Ptr _ ->
            true
        | _ -> false
    in
    assert (List.for_all params ~f:is_valid_param_type);
    assert is_valid_ret_type;
    let fun_indices =
        match idx with
        | Some idx -> `Include (Int.Set.singleton idx)
        | None -> `Include Int.Set.empty
    in
    FunPtr (Value { params; ret; fun_indices })

let make_struct name fields =
    let is_valid_field_type (_, t) =
        match t with
        | Integer _
        | Ptr _
        | ConstArray _ ->
            true
        | _ -> false
    in
    assert (List.for_all fields ~f:is_valid_field_type);
    Struct (Value { name; fields })

let make_array_inner name element_type len_type =
    let is_valid_element_type =
        match element_type with
        | Integer _
        | Ptr _
        | ConstArray _ ->
            true
        | _ -> false
    in
    let is_valid_len_type =
        match len_type with
        | Integer _ -> true
        | _ -> false
    in
    assert is_valid_element_type;
    assert is_valid_len_type;
    make_struct name [ ("len", len_type); ("[]", element_type) ]

let make_array element_type len_type =
    let name =
        match element_type with
        | Integer _ -> "int_array"
        | Ptr _ -> "ptr_array"
        | ConstArray (Value (x :: _)) -> (
            match x with
            | Integer _ -> "int_array"
            | _ -> failwith "TODO")
        | _ -> assert false
    in
    make_array_inner name element_type len_type

let make_string s =
    let len = String.length s in
    let contents = s |> String.to_list |> List.map ~f:(fun c -> Integer (Value (Char.to_int c))) in
    let el_type = ConstArray (Value contents) in
    make_array_inner "str" el_type (Integer (Value len))

let of_ast_type (ast_type : Ast.var_type) : t =
    (* FIXME actually implement *)
    match ast_type with
    | Ast.Type "i64" -> Integer All
    | Ast.Type "str" -> Ptr (make_array_inner "str" (ConstArray All) (Integer All))
    | Ast.Type "bool" -> (* TODO: actual bool type *) Integer All
    | _ -> failwithf "Unhandled AST type %s" (Ast.show_var_type ast_type) ()

let rec meet t t' =
    let meet_sub_lattice t t' ~f =
        match (t, t') with
        | All, _ -> All
        | _, All -> All
        | _, Any -> t
        | Any, _ -> t'
        | Value x, Value x' -> f x x'
    in
    match (t, t') with
    | Integer t, Integer t' ->
        Integer (meet_sub_lattice t t' ~f:(fun i i' -> if i = i' then Value i else All))
    | Tuple t, Tuple t' ->
        let l =
            meet_sub_lattice t t' ~f:(fun x x' ->
                match List.map2 x x' ~f:meet with
                | Ok l -> Value l
                | Unequal_lengths -> All)
        in
        Tuple l
    | FunPtr t, FunPtr t' ->
        let l =
            meet_sub_lattice t t' ~f:(fun fp fp' ->
                let ret = meet fp.ret fp'.ret in
                let fun_indices =
                    match (fp.fun_indices, fp'.fun_indices) with
                    | `Include s, `Include s' -> `Include (Set.union s s')
                    | `Include s, `Exclude s' -> `Exclude (Set.diff s' s)
                    | `Exclude s, `Include s' -> `Exclude (Set.diff s s')
                    | `Exclude s, `Exclude s' -> `Exclude (Set.inter s s')
                in
                match List.map2 fp.params fp'.params ~f:meet with
                | Ok params -> Value { params; ret; fun_indices }
                | Unequal_lengths -> All)
        in
        FunPtr l
    | Struct s, Struct s' ->
        let l =
            meet_sub_lattice s s' ~f:(fun s s' ->
                if not (String.equal s.name s'.name) then
                  All
                else
                  match
                    List.map2 s.fields s'.fields ~f:(fun (name, t) (name', t') ->
                        assert (String.equal name name');
                        (name, meet t t'))
                  with
                  | Ok fields -> Value { name = s.name; fields }
                  | Unequal_lengths -> assert false)
        in
        Struct l
    | Ptr p, Ptr p' -> Ptr (meet p p')
    | ConstArray a, ConstArray a' ->
        let l =
            meet_sub_lattice a a' ~f:(fun x x' ->
                match List.map2 x x' ~f:meet with
                | List.Or_unequal_lengths.Ok l -> Value l
                | List.Or_unequal_lengths.Unequal_lengths -> All)
        in
        ConstArray l
    | Control, Control -> Control
    | Control, DeadControl -> Control
    | DeadControl, Control -> Control
    | DeadControl, DeadControl -> DeadControl
    | Memory, Memory -> Memory
    | ALL, _
    | _, ALL ->
        ALL
    | ANY, _ -> t'
    | _, ANY -> t
    | Integer _, _
    | Tuple _, _
    | FunPtr _, _
    | Struct _, _
    | Ptr _, _
    | Control, _
    | DeadControl, _
    | Memory, _
    | ConstArray _, _ ->
        ALL

let rec join t t' =
    let join_sub_lattice t t' ~f =
        match (t, t') with
        | All, _ -> t'
        | _, All -> t
        | _, Any
        | Any, _ ->
            Any
        | Value x, Value x' -> f x x'
    in
    match (t, t') with
    | Integer t, Integer t' ->
        let l = join_sub_lattice t t' ~f:(fun x x' -> if x = x' then Value x else Any) in
        Integer l
    | Tuple t, Tuple t' ->
        let l =
            join_sub_lattice t t' ~f:(fun x x' ->
                match List.map2 x x' ~f:join with
                | Ok l -> Value l
                | Unequal_lengths -> Any)
        in
        Tuple l
    | FunPtr t, FunPtr t' ->
        let l =
            join_sub_lattice t t' ~f:(fun fp fp' ->
                let ret = meet fp.ret fp'.ret in
                let fun_indices =
                    match (fp.fun_indices, fp'.fun_indices) with
                    | `Include s, `Include s' -> `Include (Set.inter s s')
                    | `Include s, `Exclude s' -> `Include (Set.diff s s')
                    | `Exclude s, `Include s' -> `Include (Set.diff s' s)
                    | `Exclude s, `Exclude s' -> `Exclude (Set.union s s')
                in
                match List.map2 fp.params fp'.params ~f:meet with
                | Ok params -> Value { params; ret; fun_indices }
                | Unequal_lengths -> All)
        in
        FunPtr l
    | Struct s, Struct s' ->
        let l =
            join_sub_lattice s s' ~f:(fun s s' ->
                if not (String.equal s.name s'.name) then
                  Any
                else
                  match
                    List.map2 s.fields s'.fields ~f:(fun (name, t) (name', t') ->
                        assert (String.equal name name');
                        (name, join t t'))
                  with
                  | Ok fields -> Value { name = s.name; fields }
                  | Unequal_lengths -> assert false)
        in
        Struct l
    | Ptr p, Ptr p' -> join p p'
    | ConstArray a, ConstArray a' ->
        let l =
            join_sub_lattice a a' ~f:(fun x x' ->
                match List.map2 x x' ~f:join with
                | Ok l -> Value l
                | Unequal_lengths -> Any)
        in
        ConstArray l
    | Control, Control -> Control
    | Control, DeadControl -> DeadControl
    | DeadControl, Control -> DeadControl
    | DeadControl, DeadControl -> DeadControl
    | Memory, Memory -> Memory
    | ALL, _ -> t'
    | _, ALL -> t
    | ANY, _
    | _, ANY ->
        ANY
    | Integer _, _
    | Tuple _, _
    | FunPtr _, _
    | Struct _, _
    | Ptr _, _
    | Control, _
    | DeadControl, _
    | Memory, _
    | ConstArray _, _ ->
        ANY

let rec is_constant t =
    let is_constant_lattice = function
        | Any
        | All ->
            false
        | Value _ -> true
    in
    match t with
    | ANY
    | ALL
    | Control
    | DeadControl
    | Memory ->
        false
    | Integer x -> is_constant_lattice x
    | Tuple x -> is_constant_lattice x
    | FunPtr x -> (
        match x with
        | Any
        | All ->
            false
        | Value f -> is_constant f.ret && List.for_all f.params ~f:(fun t -> is_constant t))
    | Struct x -> (
        match x with
        | Any
        | All ->
            false
        | Value s -> List.for_all s.fields ~f:(fun (_, t) -> is_constant t))
    | Ptr p -> is_constant p
    | ConstArray x -> is_constant_lattice x

let get_fun_idx t =
    match t with
    | FunPtr (Value ({ params = _; ret = _; fun_indices } as fun_ptr)) -> (
        match fun_indices with
        | `Include s when Set.length s = 1 -> Set.choose_exn s
        | `Include _
        | `Exclude _ ->
            failwithf "Function idx couldn't be determined %s" (show_fun_ptr fun_ptr) ())
    | _ -> assert false

let is_const_array t =
    match t with
    | Struct (Value { name = _; fields }) ->
        List.exists fields ~f:(fun (name, t) ->
            if String.equal name "[]" then
              match
                t
              with
              | ConstArray (Value _) -> true
              | _ -> false
            else
              false)
    | _ -> false

let get_string t =
    match t with
    | Ptr (Struct (Value { name = _; fields })) -> (
        match
          List.find_map fields ~f:(fun (name, t) ->
              if String.equal name "[]" then
                match
                  t
                with
                | ConstArray (Value l) -> Some l
                | _ -> None
              else
                None)
        with
        | None -> None
        | Some l -> (
            match List.hd_exn l with
            | Integer _ ->
                List.map l ~f:(function
                  | Integer (Value i) -> Char.of_int_exn i
                  | _ -> assert false)
                |> String.of_char_list
                |> Option.some
            | _ -> None))
    | _ -> None

let get_offset t field =
    match t with
    | Struct (Value { name = _; fields }) ->
        List.fold_until fields ~init:0
          ~f:(fun acc (name, t) ->
            ignore t;
            if String.equal name field then
              Stop acc
            else
              Continue (acc + 8))
          ~finish:(fun _ -> assert false)
    | _ -> assert false

let is_a lhs rhs = equal (meet lhs rhs) rhs

let get_fun_param_type fun_type i =
    match fun_type with
    | FunPtr (Value { params; ret = _; fun_indices = _ }) -> List.nth_exn params i
    | _ -> assert false

let get_field_type t field_name =
    match t with
    | Struct (Value { name = _; fields }) ->
        List.find_map_exn fields ~f:(fun (name, t) ->
            if String.equal name field_name then
              Some t
            else
              None)
    | _ -> assert false

let rec human_readable t =
    match t with
    | Integer _ -> "i64"
    | Struct Any
    | Struct All ->
        "struct"
    | Struct (Value s) -> s.name
    | Ptr (Struct _ as s) ->
        (* HACK: we always represent structs as Ptr(Struct...) now so this
           makes the pretty printer look nicer, but idk if in the future i want
           to change this representation *)
        human_readable s
    | Ptr p -> "*" ^ human_readable p
    | _ -> assert false
