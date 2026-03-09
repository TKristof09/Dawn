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
    | `Include set -> Format.fprintf fmt "`Include %s" ([%derive.show: int list] (Set.to_list set))
    | `Exclude set -> Format.fprintf fmt "`Exclude %s" ([%derive.show: int list] (Set.to_list set))

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

and const_array = {
    element_type : t;
    values : t list;
  }

and integer = {
    (* TODO: this doesn't allow us to represent all (u)int64 values. But Menhir
       can't parse these anyway so we'd need to move to storing integer
       literals as strings or using some bigint library *)
    min : int;
    max : int;
    num_widens : int;
  }

and t =
    | ANY
    | Integer of integer sub_lattice
    | Bool of bool sub_lattice
    | Tuple of t list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of t
    | Struct of struct_type sub_lattice
    | ConstArray of const_array sub_lattice
    | Type of t sub_lattice
    | Void
    | Memory
    | Control
    | DeadControl
    | ALL
[@@deriving show { with_path = false }, sexp_of, equal]

let rec human_readable t =
    let human_readable_int { min; max; num_widens } =
        let bits =
            if min >= 0 then
              Int.ceil_log2 (max + 1)
            else
              let bits_neg = if min >= 0 then 0 else Int.ceil_log2 (-min) in
              let bits_pos = if max < 0 then 0 else Int.ceil_log2 (max + 1) in
              Int.max bits_neg bits_pos
        in
        Printf.sprintf "i%d" bits
    in
    match t with
    | Integer (Value i) -> human_readable_int i
    | Bool _ -> "bool"
    | Struct (Value s) -> s.name
    | Ptr (Struct _ as s) ->
        (* HACK: we always represent structs as Ptr(Struct...) now so this
           makes the pretty printer look nicer, but idk if in the future i want
           to change this representation *)
        human_readable s
    | Ptr p -> "*" ^ human_readable p
    | Void -> "void"
    | FunPtr (Value { params; ret; fun_indices }) ->
        Printf.sprintf "fun((%s) -> %s)"
          (String.concat ~sep:", " (List.map params ~f:human_readable))
          (human_readable ret)
    | Type (Value t) -> Printf.sprintf "type %s" (human_readable t)
    | Type Any
    | Type All ->
        "type"
    | ANY -> "unknown type"
    | ALL -> "invalid type"
    | _ -> failwithf "No human readable label for %s" (show t) ()

let get_top = function
    | ANY -> ANY
    | Integer _ -> Integer Any
    | Bool _ -> Bool Any
    | Tuple _ -> Tuple Any
    | FunPtr _ -> FunPtr Any
    | Ptr _ -> Ptr ANY
    | Struct _ -> Struct Any
    | ConstArray _ -> ConstArray Any
    | Type _ -> Type Any
    | Void -> Void
    | Memory -> Memory
    | Control -> DeadControl
    | DeadControl -> DeadControl
    | ALL -> ANY

let make_int ?(num_widens = 0) min max =
    if min = Int.min_value && max = Int.max_value then
      Integer All
    else if max = Int.min_value && min = Int.max_value then
      Integer Any
    else
      Integer (Value { min; max; num_widens })

let make_int_const i = Integer (Value { min = i; max = i; num_widens = 0 })
let i32 = make_int ~num_widens:3 (-1 lsl 31) (1 lsl 31)
let i16 = make_int ~num_widens:3 (-1 lsl 15) (1 lsl 15)
let i8 = make_int ~num_widens:3 (-1 lsl 7) (1 lsl 7)
let u32 = make_int ~num_widens:3 0 (1 lsl 32)
let u16 = make_int ~num_widens:3 0 (1 lsl 16)
let u8 = make_int ~num_widens:3 0 (1 lsl 8)

let make_fun_ptr ?idx params ret =
    let is_valid_param_type t =
        match t with
        | Integer _
        | Bool _
        | Ptr _
        | FunPtr _ ->
            true
        | _ -> false
    in
    let is_valid_ret_type =
        match ret with
        | Integer _
        | Bool _
        | Ptr _
        | FunPtr _
        | Struct _
        | Void ->
            true
        | _ -> false
    in
    assert (List.for_all params ~f:is_valid_param_type);
    assert is_valid_ret_type;
    let fun_indices =
        match idx with
        | Some idx -> `Include (Int.Set.singleton idx)
        | None -> `Exclude Int.Set.empty
    in
    let params =
        List.map params ~f:(fun t ->
            match t with
            | Struct _ -> Ptr t
            | _ -> t)
    in
    let ret =
        match ret with
        | Struct _ -> Ptr ret
        | _ -> ret
    in
    FunPtr (Value { params; ret; fun_indices })

let make_struct name fields =
    let is_valid_field_type (_, t) =
        match t with
        | Integer _
        | Bool _
        | Ptr _
        | ConstArray _
        | FunPtr _ ->
            true
        | _ -> false
    in
    assert (List.for_all fields ~f:is_valid_field_type);
    Struct (Value { name; fields })

let make_array_inner name element_type len_type =
    let is_valid_element_type =
        match element_type with
        | Integer _
        | Bool _
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
        | Integer _ -> human_readable element_type ^ "_array"
        | Bool _ -> "bool_array"
        | Ptr _ -> "ptr_array"
        | ConstArray (Value { element_type; values = _ }) -> (
            match element_type with
            | Integer _ -> "const_" ^ human_readable element_type ^ "_array"
            | _ -> failwith "TODO")
        | _ -> assert false
    in
    make_array_inner name element_type len_type

let make_string s =
    let len = String.length s in
    let values = s |> String.to_list |> List.map ~f:(fun c -> make_int_const (Char.to_int c)) in
    let typ = ConstArray (Value { element_type = u8; values }) in
    make_array_inner "str" typ (make_int_const len)

let rec of_ast_type (ast_type : Ast.var_type) : t =
    let try_parse_int s =
        if Char.equal s.[0] 'i' then
          Int.of_string_opt (String.drop_prefix s 1) |> Option.map ~f:(fun bits -> `Int bits)
        else if Char.equal s.[0] 'u' then
          Int.of_string_opt (String.drop_prefix s 1) |> Option.map ~f:(fun bits -> `UInt bits)
        else
          None
    in
    (* FIXME actually implement *)
    match ast_type with
    | Type s -> (
        match try_parse_int s with
        | Some (`UInt bits) -> make_int 0 (1 lsl bits)
        | Some (`Int bits) -> make_int (-1 lsl (bits - 1)) (1 lsl (bits - 1))
        | None -> (
            match s with
            | "str" ->
                Ptr
                  (make_array_inner "str"
                     (ConstArray (Value { element_type = u8; values = [] }))
                     (Integer All))
            | "bool" -> Bool All
            | "void" -> Void
            | _ -> failwithf "Unhandled AST type %s" (Ast.show_var_type ast_type) ()))
    | Fn (ret, params) ->
        let ret = of_ast_type ret in
        let params = List.map params ~f:of_ast_type in
        make_fun_ptr params ret
    | Struct fields -> make_struct "" (List.map fields ~f:(fun (n, t) -> (n, of_ast_type t)))
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
        Integer
          (meet_sub_lattice t t' ~f:(fun i i' ->
               let min_range = min i.min i'.min in
               let max_range = max i.max i'.max in
               if min_range = Int.min_value && max_range = Int.max_value then
                 All
               else if max_range = Int.min_value && min_range = Int.max_value then
                 Any
               else
                 Value
                   { min = min_range; max = max_range; num_widens = max i.num_widens i'.num_widens }))
    | Bool t, Bool t' ->
        Bool (meet_sub_lattice t t' ~f:(fun i i' -> if Bool.equal i i' then Value i else All))
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
                let element_type = meet x.element_type x'.element_type in
                if equal element_type ALL then
                  All
                else
                  match
                    List.map2 x.values x'.values ~f:meet
                  with
                  | List.Or_unequal_lengths.Ok l -> Value { element_type; values = l }
                  | List.Or_unequal_lengths.Unequal_lengths -> All)
        in
        ConstArray l
    | Type t, Type t' ->
        let l =
            meet_sub_lattice t t' ~f:(fun t t' ->
                match meet t t' with
                | ALL -> All
                | ANY -> Any
                | x -> Value x)
        in
        Type l
    | Control, Control -> Control
    | Control, DeadControl -> Control
    | DeadControl, Control -> Control
    | DeadControl, DeadControl -> DeadControl
    | Memory, Memory -> Memory
    | Void, Void -> Void
    | ALL, _
    | _, ALL ->
        ALL
    | ANY, _ -> t'
    | _, ANY -> t
    | Integer _, _
    | Bool _, _
    | Tuple _, _
    | FunPtr _, _
    | Struct _, _
    | Ptr _, _
    | Control, _
    | DeadControl, _
    | Memory, _
    | ConstArray _, _
    | Type _, _
    | Void, _ ->
        ALL

let is_a lhs rhs = equal (meet lhs rhs) rhs

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
        Integer
          (join_sub_lattice t t' ~f:(fun i i' ->
               let min_range = max i.min i'.min in
               let max_range = min i.max i'.max in
               if min_range = Int.min_value && max_range = Int.max_value then
                 All
               else if max_range = Int.min_value && min_range = Int.max_value then
                 Any
               else
                 Value
                   { min = min_range; max = max_range; num_widens = min i.num_widens i'.num_widens }))
    | Bool t, Bool t' ->
        let l = join_sub_lattice t t' ~f:(fun x x' -> if Bool.equal x x' then Value x else Any) in
        Bool l
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
                let element_type = join x.element_type x'.element_type in
                if equal element_type ANY then
                  Any
                else
                  match
                    List.map2 x.values x'.values ~f:join
                  with
                  | List.Or_unequal_lengths.Ok l -> Value { element_type; values = l }
                  | List.Or_unequal_lengths.Unequal_lengths -> Any)
        in
        ConstArray l
    | Type t, Type t' ->
        let l =
            join_sub_lattice t t' ~f:(fun t t' ->
                match join t t' with
                | ALL -> All
                | ANY -> Any
                | x -> Value x)
        in
        Type l
    | Control, Control -> Control
    | Control, DeadControl -> DeadControl
    | DeadControl, Control -> DeadControl
    | DeadControl, DeadControl -> DeadControl
    | Memory, Memory -> Memory
    | Void, Void -> Void
    | ALL, _ -> t'
    | _, ALL -> t
    | ANY, _
    | _, ANY ->
        ANY
    | Integer _, _
    | Bool _, _
    | Tuple _, _
    | FunPtr _, _
    | Struct _, _
    | Ptr _, _
    | Control, _
    | DeadControl, _
    | Memory, _
    | ConstArray _, _
    | Type _, _
    | Void, _ ->
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
    | Integer (Value { min; max; num_widens = _ }) -> min = max
    | Integer _ -> false
    | Bool x -> is_constant_lattice x
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
    | Type x -> is_constant_lattice x
    | Void -> true

let get_fun_idx t =
    match t with
    | FunPtr (Value { params = _; ret = _; fun_indices }) -> (
        match fun_indices with
        | `Include s when Set.length s = 1 -> Some (Set.choose_exn s)
        | `Include _
        | `Exclude _ ->
            None)
    | _ -> failwithf "Invalid arg: %s" (show t) ()

let iter_fun_indices t universe ~f =
    match t with
    | FunPtr (Value { params = _; ret = _; fun_indices }) ->
        let indices =
            match fun_indices with
            | `Include s -> Set.inter universe s
            | `Exclude s -> Set.diff universe s
        in
        Set.iter indices ~f
    | _ -> failwithf "Invalid arg: %s" (show t) ()

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
        | Some { element_type; values } when is_a element_type u8 -> (
            match List.hd_exn values with
            | Integer _ ->
                List.map values ~f:(function
                  | Integer (Value { min; max; num_widens = _ }) when min = max ->
                      Char.of_int_exn min
                  | _ -> assert false)
                |> String.of_char_list
                |> Option.some
            | _ -> None)
        | _ -> None)
    | _ -> None

let rec get_offset t field =
    match t with
    | Struct (Value { name = _; fields }) ->
        List.fold_until fields ~init:0
          ~f:(fun acc (name, t) ->
            (* TODO: use t's size *)
            ignore t;
            if String.equal name field then
              Stop (Some acc)
            else
              Continue (acc + 8))
          ~finish:(fun _ -> None)
    | Ptr (Struct _ as s) -> get_offset s field
    | _ -> None

let rec get_field_type t field_name =
    match t with
    | Struct (Value { name = _; fields }) ->
        List.find_map fields ~f:(fun (name, t) ->
            if String.equal name field_name then
              Some t
            else
              None)
    | Ptr (Struct _ as s) -> get_field_type s field_name
    | _ -> None

let rec get_size t =
    match t with
    | Integer (Value { min; max; num_widens = _ }) ->
        (* Integers get rounded oup to nearest multiple of 8bit size, so e.g. i12 would be stored as i16 *)
        let bits =
            if min >= 0 then
              Int.ceil_log2 (max + 1)
            else
              let bits_neg = if min >= 0 then 0 else Int.ceil_log2 (-min) in
              let bits_pos = if max < 0 then 0 else Int.ceil_log2 (max + 1) in
              Int.max bits_neg bits_pos
        in
        (bits + 7) / 8
    | Integer _ -> 8
    | Bool _ -> 1
    | Struct (Value s) ->
        List.sum
          (module Int)
          s.fields
          ~f:(fun (name, t) ->
            if String.equal name "[]" then
              failwith "todo array size"
            else
              get_size t)
    | Ptr _
    | FunPtr _ ->
        8
    | _ -> failwithf "todo: %s" (show t) ()

let get_integer_const_exn = function
    | Integer (Value { min; max; num_widens = _ }) ->
        if min = max then
          min
        else
          raise (Invalid_argument "Not an integer constant")
    | _ -> raise (Invalid_argument "Not an integer constant")
