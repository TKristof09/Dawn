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

type integer = {
    (* TODO: probably want to keep track of whether the types is actually unsigned or if it's signed but just happens to have min >= 0 *)
    (* TODO: decide and document better the inclusiveness of endpoints *)
    min : Z.t;
    max : Z.t;
    num_widens : int;
    fixed_width : int option; (* in bits *)
  }

let pp_integer fmt { min; max; num_widens; fixed_width } =
    match fixed_width with
    | Some w ->
        Format.fprintf fmt "[%s, %s] (width=%d, widens=%d)" (Z.to_string min) (Z.to_string max) w
          num_widens
    | None ->
        Format.fprintf fmt "[%s, %s] (widens=%d)" (Z.to_string min) (Z.to_string max) num_widens

let sexp_of_integer { min; max; num_widens; fixed_width } =
    Sexp.List
      [
        Sexp.List [ Sexp.Atom "min"; Sexp.Atom (Z.to_string min) ];
        Sexp.List [ Sexp.Atom "max"; Sexp.Atom (Z.to_string max) ];
        Sexp.List [ Sexp.Atom "num_widens"; sexp_of_int num_widens ];
        Sexp.List [ Sexp.Atom "fixed_width"; sexp_of_option sexp_of_int fixed_width ];
      ]

let equal_integer { min; max; num_widens; fixed_width }
    { min = min'; max = max'; num_widens = num_widens'; fixed_width = fixed_width' } =
    Z.equal min min'
    && Z.equal max max'
    && num_widens = num_widens'
    && Option.equal Int.equal fixed_width fixed_width'

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
    let human_readable_int { min; max; num_widens; fixed_width } =
        (* Format.asprintf "%a" pp_integer { min; max; num_widens; fixed_width } *)
        let bits =
            match fixed_width with
            | Some w -> w
            | None ->
                if Z.geq min Z.zero then
                  Z.numbits (Z.add max Z.one)
                else
                  let bits_neg = if Z.geq min Z.zero then 0 else Z.numbits min in
                  let bits_pos = if Z.lt max Z.zero then 0 else Z.numbits (Z.add max Z.one) in
                  1 + Int.max bits_neg bits_pos
        in
        Printf.sprintf "i%d" (Int.max 1 bits)
    in
    match t with
    | Integer (Value i) -> human_readable_int i
    | Integer _ -> "integer"
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

let int64_min = Z.neg (Z.pow (Z.of_int 2) 63)
let uint64_max = Z.sub (Z.pow (Z.of_int 2) 64) Z.one

let make_int ?(num_widens = 0) ?fixed_width min max =
    if Z.equal min int64_min && Z.equal max uint64_max then
      Integer All
    else if Z.equal max int64_min && Z.equal min uint64_max then
      Integer Any
    else
      Integer (Value { min; max; num_widens; fixed_width })

let make_int_const ?fixed_width i =
    Integer (Value { min = i; max = i; num_widens = 0; fixed_width })

let signed_bounds w =
    let pow = Z.pow (Z.of_int 2) (w - 1) in
    (Z.neg pow, Z.sub pow Z.one)

let unsigned_bounds w = (Z.zero, Z.sub (Z.pow (Z.of_int 2) w) Z.one)

let make_int_fixed ~fixed_width ~num_widens bounds_fn =
    let min, max = bounds_fn fixed_width in
    make_int ~fixed_width ~num_widens min max

let i64 = make_int_fixed ~fixed_width:64 ~num_widens:3 signed_bounds
let i32 = make_int_fixed ~fixed_width:32 ~num_widens:3 signed_bounds
let i16 = make_int_fixed ~fixed_width:16 ~num_widens:3 signed_bounds
let i8 = make_int_fixed ~fixed_width:8 ~num_widens:3 signed_bounds
let u64 = make_int_fixed ~fixed_width:64 ~num_widens:3 unsigned_bounds
let u32 = make_int_fixed ~fixed_width:32 ~num_widens:3 unsigned_bounds
let u16 = make_int_fixed ~fixed_width:16 ~num_widens:3 unsigned_bounds
let u8 = make_int_fixed ~fixed_width:8 ~num_widens:3 unsigned_bounds

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
    let len = String.length s |> Z.of_int in
    let values =
        s
        |> String.to_list
        |> List.map ~f:(fun c -> make_int_const ~fixed_width:8 (Char.to_int c |> Z.of_int))
    in
    let typ = ConstArray (Value { element_type = u8; values }) in
    make_array_inner "str" typ (make_int_const ~fixed_width:64 len)

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
        | Some (`UInt bits) -> make_int_fixed ~fixed_width:bits ~num_widens:3 unsigned_bounds
        | Some (`Int bits) -> make_int_fixed ~fixed_width:bits ~num_widens:3 signed_bounds
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
               let min_range = Z.min i.min i'.min in
               let max_range = Z.max i.max i'.max in
               let combined_width =
                   match (i.fixed_width, i'.fixed_width) with
                   | None, None -> None
                   | Some w, None
                   | None, Some w ->
                       Some w
                   | Some w, Some w' -> Some (Int.max w w')
               in
               if Z.equal min_range int64_min && Z.equal max_range uint64_max then
                 All
               else if Z.equal max_range int64_min && Z.equal min_range uint64_max then
                 Any
               else
                 Value
                   {
                     min = min_range;
                     max = max_range;
                     num_widens = Int.max i.num_widens i'.num_widens;
                     fixed_width = combined_width;
                   }))
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
        (* TODO: review this code (and the join for constarrays). Is it fine to
           loose the values list when meeting/joining two different
           constarrays? I think so since it will no longer be compile time
           constant anyhow but need to think more *)
        let l =
            meet_sub_lattice a a' ~f:(fun x x' ->
                if List.equal Poly.equal x.values x'.values then
                  Value x
                else
                  let element_type = meet x.element_type x'.element_type in
                  if equal element_type ALL then
                    All
                  else
                    Value { element_type; values = [] })
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
               let min_range = Z.max i.min i'.min in
               let max_range = Z.min i.max i'.max in
               let combined_width =
                   (* TODO: should this logic be the same in both meet and join? Need to think more about it *)
                   match (i.fixed_width, i'.fixed_width) with
                   | None, None -> None
                   | Some w, None
                   | None, Some w ->
                       Some w
                   | Some w, Some w' -> Some (Int.max w w')
               in
               if Z.equal min_range int64_min && Z.equal max_range uint64_max then
                 All
               else if Z.equal max_range int64_min && Z.equal min_range uint64_max then
                 Any
               else
                 Value
                   {
                     min = min_range;
                     max = max_range;
                     num_widens = Int.min i.num_widens i'.num_widens;
                     fixed_width = combined_width;
                   }))
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
                if List.equal Poly.equal x.values x'.values then
                  Value x
                else
                  let element_type = join x.element_type x'.element_type in
                  if equal element_type ANY then
                    Any
                  else
                    Value { element_type; values = [] })
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
    | Integer (Value { min; max; num_widens = _ }) -> Z.equal min max
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
                  | Integer (Value { min; max; num_widens = _; fixed_width = _ })
                    when Z.equal min max ->
                      (* TODO: nicer error message than just Char.of_int_exn i guess *)
                      Z.to_int min |> Char.of_int_exn
                  | _ -> assert false)
                |> String.of_char_list
                |> Option.some
            | _ -> None)
        | _ -> None)
    | _ -> None

let rec get_size t =
    match t with
    | Integer (Value { min; max; num_widens = _; fixed_width }) ->
        (* Integers get rounded up to nearest of 8/16/32/64, so e.g. i12 would be stored as i16 *)
        let bits =
            match fixed_width with
            | Some w -> w
            | None ->
                if Z.geq min Z.zero then
                  Z.numbits (Z.add max Z.one)
                else
                  let bits_neg = if Z.geq min Z.zero then 0 else Z.numbits min in
                  let bits_pos = if Z.lt max Z.zero then 0 else Z.numbits (Z.add max Z.one) in
                  1 + Int.max bits_neg bits_pos
        in
        let bits = Int.max 1 bits in
        let bits =
            if bits <= 8 then
              8
            else if bits <= 16 then
              16
            else if bits <= 32 then
              32
            else
              64
        in
        bits / 8
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
    | ConstArray (Value { element_type; values = _ }) -> get_size element_type
    | Ptr _
    | FunPtr _ ->
        8
    | _ -> failwithf "todo: %s" (show t) ()

let rec get_offset t field =
    match t with
    | Struct (Value { name = _; fields }) ->
        List.fold_until fields ~init:0
          ~f:(fun acc (name, t) ->
            if String.equal name field then
              Stop (Some acc)
            else
              Continue (acc + get_size t))
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

let get_integer_const_exn = function
    | Integer (Value { min; max; num_widens = _ }) ->
        if Z.equal min max then
          min
        else
          raise (Invalid_argument "Not an integer constant")
    | _ -> raise (Invalid_argument "Not an integer constant")

let widen_int t min_type =
    match t with
    | Integer (Value { min; max; num_widens; fixed_width }) -> (
        if num_widens < 3 then
          Integer (Value { min; max; num_widens = num_widens + 1; fixed_width })
        else
          match
            fixed_width
          with
          | Some w ->
              [%log.debug "Have fixed width %d ignoring min_type" w];
              let is_unsigned = Z.geq min Z.zero in
              make_int_fixed ~num_widens:3 ~fixed_width:w
                (if is_unsigned then unsigned_bounds else signed_bounds)
          | None -> min_type)
    | _ -> assert false

let rec is_high = function
    | ANY
    | Integer Any
    | Bool Any
    | Tuple Any
    | FunPtr Any
    | Struct Any
    | ConstArray Any
    | Type Any
    | DeadControl ->
        true
    | Ptr t -> is_high t
    | _ -> false
