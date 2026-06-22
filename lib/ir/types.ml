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
    Z.equal min min' && Z.equal max max' && Option.equal Int.equal fixed_width fixed_width'

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

and arr = {
    (* Invariant Array.for_all values ~f:(fun v -> is_a v element_type) should always hold *)
    (* TODO: since we want the invariant to hold we should make this type abstract from outside *)
    element_type : t;
    values : t array;
  }

and t =
    | ANY
    | Integer of integer sub_lattice
    | Bool of bool sub_lattice
    | Tuple of t list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of t
    | Struct of struct_type sub_lattice
    | Trait of struct_type sub_lattice
    | Array of arr sub_lattice
    | ConstArray of arr sub_lattice
    | Type of t sub_lattice
    | Void
    | Memory
    | Control
    | DeadControl
    | ALL
[@@deriving show { with_path = false }, sexp_of]

let get_top = function
    | ANY -> ANY
    | Integer _ -> Integer Any
    | Bool _ -> Bool Any
    | Tuple _ -> Tuple Any
    | FunPtr _ -> FunPtr Any
    | Ptr _ -> Ptr ANY
    | Struct _ -> Struct Any
    | Trait _ -> Trait Any
    | ConstArray _ -> ConstArray Any
    | Array _ -> Array Any
    | Type _ -> Type Any
    | Void -> Void
    | Memory -> Memory
    | Control -> DeadControl
    | DeadControl -> DeadControl
    | ALL -> ANY

let get_bottom = function
    | ANY -> ALL
    | Integer _ -> Integer All
    | Bool _ -> Bool All
    | Tuple _ -> Tuple All
    | FunPtr _ -> FunPtr All
    | Ptr _ -> Ptr ALL
    | Struct _ -> Struct All
    | Trait _ -> Trait All
    | ConstArray _ -> ConstArray All
    | Array _ -> Array All
    | Type _ -> Type All
    | Void -> Void
    | Memory -> Memory
    | Control -> DeadControl
    | DeadControl -> DeadControl
    | ALL -> ALL

let rec equal a b =
    let equal_fun_ptr a b =
        let equal_indices a b =
            match (a, b) with
            | `Include s, `Include s' -> Int.Set.equal s s'
            | `Exclude s, `Exclude s' -> Int.Set.equal s s'
            | _, _ -> false
        in
        equal a.ret b.ret
        && List.equal equal a.params b.params
        && equal_indices a.fun_indices b.fun_indices
    in
    let equal_struct a b =
        String.equal a.name b.name
        && List.equal (fun (n, t) (n', t') -> String.equal n n' && equal t t') a.fields b.fields
    in
    let equal_arr a b =
        let shorter =
            if Array.length a.values < Array.length b.values then a.values else b.values
        in
        let longer = if Array.length a.values < Array.length b.values then b.values else a.values in
        equal a.element_type b.element_type
        && Array.foldi longer ~init:true ~f:(fun i acc x ->
            if i < Array.length shorter then
              acc && equal shorter.(i) x
            else
              acc && equal x (get_bottom x))
    in
    match (a, b) with
    | ANY, ANY -> true
    | ANY, _ -> false
    | _, ANY -> false
    | Integer a, Integer b -> equal_sub_lattice equal_integer a b
    | Integer _, _ -> false
    | _, Integer _ -> false
    | Bool a, Bool b -> equal_sub_lattice Bool.equal a b
    | Bool _, _ -> false
    | _, Bool _ -> false
    | Tuple a, Tuple b -> equal_sub_lattice (List.equal equal) a b
    | Tuple _, _ -> false
    | _, Tuple _ -> false
    | FunPtr a, FunPtr b -> equal_sub_lattice equal_fun_ptr a b
    | FunPtr _, _ -> false
    | _, FunPtr _ -> false
    | Ptr a, Ptr b -> equal a b
    | Ptr _, _ -> false
    | _, Ptr _ -> false
    | Struct a, Struct b -> equal_sub_lattice equal_struct a b
    | Struct _, _ -> false
    | _, Struct _ -> false
    | Trait a, Trait b -> equal_sub_lattice equal_struct a b
    | Trait _, _ -> false
    | _, Trait _ -> false
    | Array a, Array b -> equal_sub_lattice equal_arr a b
    | Array _, _ -> false
    | _, Array _ -> false
    | ConstArray a, ConstArray b -> equal_sub_lattice equal_arr a b
    | ConstArray _, _ -> false
    | _, ConstArray _ -> false
    | Type a, Type b -> equal_sub_lattice equal a b
    | Type _, _ -> false
    | _, Type _ -> false
    | Void, Void -> true
    | Void, _ -> false
    | _, Void -> false
    | Memory, Memory -> true
    | Memory, _ -> false
    | _, Memory -> false
    | Control, Control -> true
    | Control, _ -> false
    | _, Control -> false
    | DeadControl, DeadControl -> true
    | DeadControl, _ -> false
    | _, DeadControl -> false
    | ALL, ALL -> true

let rec human_readable t =
    let human_readable_int { min; max; num_widens; fixed_width } =
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
        Printf.sprintf "%c%d" (if Z.geq min Z.zero then 'u' else 'i') (Int.max 1 bits)
    in
    match t with
    | Integer (Value i) -> human_readable_int i
    | Integer _ -> "integer"
    | Bool _ -> "bool"
    | Struct (Value s) -> s.name
    | Struct _ -> "struct"
    | Trait (Value s) ->
        (* drop leading $ *)
        String.drop_prefix s.name 1
    | Trait _ -> "trait"
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
    | Array (Value { element_type; values = _ })
    | ConstArray (Value { element_type; values = _ }) ->
        human_readable element_type ^ " array"
    | ANY -> "unknown type"
    | ALL -> "invalid type"
    | _ -> failwithf "No human readable label for %s" (show t) ()

let get_fun_idx t =
    match t with
    | FunPtr (Value { params = _; ret = _; fun_indices }) -> (
        match fun_indices with
        | `Include s when Set.length s = 1 -> Some (Set.choose_exn s)
        | `Include _
        | `Exclude _ ->
            None)
    | _ -> failwithf "Invalid arg: %s" (show t) ()

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
        | Value f -> get_fun_idx t |> Option.is_some)
    | Struct x
    | Trait x -> (
        match x with
        | Any
        | All ->
            false
        | Value s -> List.for_all s.fields ~f:(fun (_, t) -> is_constant t))
    | Ptr p -> is_constant p
    | Array a
    | ConstArray a -> (
        match a with
        | Any
        | All ->
            false
        | Value { element_type; values } ->
            is_constant element_type && Array.for_all values ~f:is_constant)
    | Type x -> is_constant_lattice x
    | Void -> true

let get_integer_const_exn = function
    | Integer (Value { min; max; num_widens = _ }) ->
        if Z.equal min max then
          min
        else
          raise (Invalid_argument "Not an integer constant")
    | _ -> raise (Invalid_argument "Not an integer constant")

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
        | FunPtr _
        | Struct _
        | Trait _ ->
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

    let params, ret =
        match ret with
        | Struct _ ->
            (* If return value is big caller allocates stack space and passes in ptr as first arg *)
            (* TODO it should actually depends on size of ret to allow small
               structs returned as reg but that would be a bunch more work i
               think *)
            (Ptr ret :: params, Ptr ret)
        | _ -> (params, ret)
    in
    FunPtr (Value { params; ret; fun_indices })

let make_struct name fields =
    let is_valid_field_type (_, t) =
        match t with
        | Integer _
        | Bool _
        | Ptr _
        | ConstArray _
        | Array _
        | FunPtr _
        | Struct _
        | Void ->
            true
        | _ -> false
    in
    assert (List.for_all fields ~f:is_valid_field_type);
    Struct (Value { name; fields })

let make_trait name fields =
    let is_valid_field_type (_, t) =
        match t with
        | FunPtr _ -> true
        | _ -> false
    in
    assert (List.for_all fields ~f:is_valid_field_type);
    Trait (Value { name; fields })

let make_array_inner name element_type len_type =
    let is_valid_element_type =
        match element_type with
        | Array _
        | ConstArray _ ->
            true
        | Ptr _ ->
            (* This is for slices, would probably make sense to put these in a separate function though  *)
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
    let values =
        if is_constant len_type then
          Array.create ~len:(get_integer_const_exn len_type |> Z.to_int) (get_top element_type)
        else
          [||]
    in
    let element_type = Array (Value { element_type; values }) in
    make_array_inner name element_type len_type

let make_string s =
    let len = String.length s |> Z.of_int in
    let values =
        s
        |> String.to_array
        |> Array.map ~f:(fun c -> make_int_const ~fixed_width:8 (Char.to_int c |> Z.of_int))
    in
    let typ = ConstArray (Value { element_type = u8; values }) in
    make_array_inner "str" (Ptr typ) (make_int_const ~fixed_width:64 len)

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
                (* This is just a slice but want a specific name *)
                make_array_inner "str"
                  (Ptr (Array (Value { element_type = u8; values = [||] })))
                  u64
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
                       (* if bigger than previous fixed width  then remove the
                          fixed width since we'll need more anyway *)
                       if Z.numbits max_range > w || Z.numbits min_range > w then
                         None
                       else
                         Some w
                   | Some w, Some w' ->
                       let w = Int.max w w' in
                       (* if bigger than previous fixed width  then remove the
                          fixed width since we'll need more anyway *)
                       if Z.numbits max_range > w || Z.numbits min_range > w then
                         None
                       else
                         Some w
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
    | Struct s, Struct s' -> (
        (* We don't use the meet_sub_lattice helper because we want meet s s'
           be a trait if the structs have different names but have traits that
           are implemented in both structs *)
        match (s, s') with
        | All, _ -> Struct All
        | _, All -> Struct All
        | _, Any -> t
        | Any, _ -> t'
        | Value s, Value s' -> (
            if not (String.equal s.name s'.name) then
              (* different structs, but they might have common implemented traits. We want those *)
              let s_trait_impls =
                  List.filter s.fields ~f:(fun (n, _) -> String.is_prefix n ~prefix:"$")
                  |> String.Map.of_alist_reduce ~f:meet
              in
              let s'_trait_impls =
                  List.filter s'.fields ~f:(fun (n, _) -> String.is_prefix n ~prefix:"$")
                  |> String.Map.of_alist_reduce ~f:meet
              in
              let common =
                  Map.merge s_trait_impls s'_trait_impls ~f:(fun ~key -> function
                    | `Both (t, t') -> Some (meet t t')
                    | _ -> None)
              in
              if Map.is_empty common then
                Struct All
              else
                (* just concat the trait names together for now (so we get $T1+$T2+$T3...)
                      *)
                let name =
                    Map.fold common ~init:[] ~f:(fun ~key ~data acc ->
                        if equal data Void && String.count key ~f:(Char.equal '$') = 1 then
                          key :: acc
                        else
                          acc)
                in
                let name = String.concat ~sep:"+" name in
                Trait (Value { name; fields = Map.to_alist common })
            else
              match
                List.map2 s.fields s'.fields ~f:(fun (name, t) (name', t') ->
                    assert (String.equal name name');
                    (name, meet t t'))
              with
              | Ok fields -> Struct (Value { name = s.name; fields })
              | Unequal_lengths -> assert false))
    | Trait t, Trait t' ->
        let l =
            meet_sub_lattice t t' ~f:(fun t t' ->
                if not (String.equal t.name t'.name) then
                  All
                else
                  match
                    List.map2 t.fields t'.fields ~f:(fun (name, t) (name', t') ->
                        assert (String.equal name name');
                        (name, meet t t'))
                  with
                  | Ok fields -> Value { name = t.name; fields }
                  | Unequal_lengths -> assert false)
        in
        Trait l
    | Trait t, Struct s
    | Struct s, Trait t ->
        (* Traits are "below" structs, meeting struct with trait gives back the trait if it's implemented or we fall all way to Trait All*)
        let l =
            meet_sub_lattice s t ~f:(fun s t ->
                let rec aux s_fields t_fields found_name =
                    match (s_fields, t_fields) with
                    | [], [] -> Ok []
                    | [], _ -> Error ()
                    | _, [] -> Ok []
                    | (name, typ) :: ts, _ when String.equal name ("$" ^ t.name) ->
                        aux ts t_fields true
                    | (name_s, typ_s) :: ts, (name_t, typ_t) :: tt when found_name ->
                        assert (String.equal name_s ("$" ^ t.name ^ "$" ^ name_t));
                        Result.map (aux ts tt found_name) ~f:(fun rest ->
                            (name_t, meet typ_s typ_t) :: rest)
                    | _ :: t, _ -> aux t t_fields found_name
                in

                match aux s.fields t.fields false with
                | Ok fields -> Value { name = t.name; fields }
                | Error _ -> All)
        in
        Trait l
    | Ptr p, Ptr p' -> Ptr (meet p p')
    | ConstArray a, ConstArray a' ->
        (* TODO: review this code (and the join for constarrays). Is it fine to
           loose the values list when meeting/joining two different
           constarrays? I think so since it will no longer be compile time
           constant anyhow but need to think more *)
        let l =
            meet_sub_lattice a a' ~f:(fun x x' ->
                if Array.equal Poly.equal x.values x'.values then
                  Value x
                else
                  let element_type = meet x.element_type x'.element_type in
                  if equal element_type ALL then
                    All
                  else
                    Value { element_type; values = [||] })
        in
        ConstArray l
    | Array a, Array a' ->
        let l =
            meet_sub_lattice a a' ~f:(fun x x' ->
                let element_type = meet x.element_type x'.element_type in
                let padded_map2 a b ~f ~pad =
                    let len_a = Array.length a in
                    let len_b = Array.length b in
                    let len = max len_a len_b in
                    Array.init len ~f:(fun i ->
                        let x = if i < len_a then a.(i) else pad in
                        let y = if i < len_b then b.(i) else pad in
                        f x y)
                in
                if equal element_type ALL then
                  All
                else
                  Value
                    {
                      element_type;
                      values = padded_map2 x.values x'.values ~f:meet ~pad:(get_bottom element_type);
                    })
        in
        Array l
    | Array a, ConstArray c
    | ConstArray c, Array a ->
        (* demote the constant array into normal array *)
        meet (Array a) (Array c)
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
    | Trait _, _
    | Ptr _, _
    | Control, _
    | DeadControl, _
    | Memory, _
    | ConstArray _, _
    | Array _, _
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
    | Trait t, Trait t' ->
        let l =
            join_sub_lattice t t' ~f:(fun t t' ->
                if not (String.equal t.name t'.name) then
                  Any
                else
                  match
                    List.map2 t.fields t'.fields ~f:(fun (name, t) (name', t') ->
                        assert (String.equal name name');
                        (name, meet t t'))
                  with
                  | Ok fields -> Value { name = t.name; fields }
                  | Unequal_lengths -> assert false)
        in
        Trait l
    | Trait t, Struct s
    | Struct s, Trait t ->
        failwithf "TODO %s" __LOC__ ()
    | Ptr p, Ptr p' -> Ptr (join p p')
    | Array a, Array a' ->
        let l =
            join_sub_lattice a a' ~f:(fun x x' ->
                let element_type = join x.element_type x'.element_type in
                let padded_map2 a b ~f ~pad =
                    let len_a = Array.length a in
                    let len_b = Array.length b in
                    let len = max len_a len_b in
                    Array.init len ~f:(fun i ->
                        let x = if i < len_a then a.(i) else pad in
                        let y = if i < len_b then b.(i) else pad in
                        f x y)
                in
                if equal element_type ANY then
                  Any
                else
                  Value
                    {
                      element_type;
                      values = padded_map2 x.values x'.values ~f:join ~pad:(get_top element_type);
                    })
        in
        Array l
    | ConstArray a, ConstArray a' ->
        let l =
            join_sub_lattice a a' ~f:(fun x x' ->
                if Array.equal Poly.equal x.values x'.values then
                  Value x
                else
                  let element_type = join x.element_type x'.element_type in
                  if equal element_type ANY then
                    Any
                  else
                    Value { element_type; values = [||] })
        in
        ConstArray l
    | Array a, ConstArray c
    | ConstArray c, Array a ->
        failwithf "%s TODO" __LOC__ ()
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
    | Trait _, _
    | Ptr _, _
    | Control, _
    | DeadControl, _
    | Memory, _
    | Array _, _
    | ConstArray _, _
    | Type _, _
    | Void, _ ->
        ANY

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
              | Ptr (ConstArray (Value _)) -> true
              | _ -> false
            else
              false)
    | _ -> false

let get_string t =
    match t with
    | Ptr (Struct (Value { name = _; fields }))
    | Struct (Value { name = _; fields }) -> (
        match
          List.find_map fields ~f:(fun (name, t) ->
              if String.equal name "[]" then
                match
                  t
                with
                | ConstArray (Value l) -> Some l
                | Ptr (ConstArray (Value l)) -> Some l
                | _ -> None
              else
                None)
        with
        | None -> None
        | Some { element_type; values } when is_a element_type u8 ->
            Array.map values ~f:(function
              | Integer (Value { min; max; num_widens = _; fixed_width = _ }) when Z.equal min max
                ->
                  (* TODO: nicer error message than just Char.of_int_exn i guess *)
                  Z.to_int min |> Char.of_int_exn
              | _ -> assert false)
            |> String.of_array
            |> Option.some
        | _ -> None)
    | _ -> None

let rec get_field_type t field_name =
    match t with
    | Struct (Value { name = _; fields })
    | Trait (Value { name = _; fields }) ->
        List.find_map fields ~f:(fun (name, t) ->
            if String.equal name field_name then
              Some t
            else
              None)
    | Ptr (Struct _ as s) -> get_field_type s field_name
    | _ -> None

let get_array_element_type t =
    match t with
    | Array (Value { element_type; values = _ })
    | ConstArray (Value { element_type; values = _ }) ->
        element_type
    | _ -> assert false

let rec get_size t =
    match t with
    | Integer (Value { min; max; num_widens = _; fixed_width }) ->
        (* Integers get rounded up to nearest of 8/16/32/64, so e.g. i12 would be stored as i16 *)
        let bits =
            match fixed_width with
            | Some w -> w
            | None ->
                if Z.geq min Z.zero then
                  Z.numbits max
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
              match
                t
              with
              | Ptr _ ->
                  (* slices *)
                  (* TODO: how to differentiate slice from static length array of pointers? *)
                  8
              | _ ->
                  let len_t =
                      List.find_map_exn s.fields ~f:(fun (name, t) ->
                          if String.equal name "len" then Some t else None)
                  in
                  if is_constant len_t then
                    let count = get_integer_const_exn len_t in
                    let el_size = get_size t in
                    Z.to_int count * el_size
                  else
                    failwith "Array with non compile time known length"
            else
              get_size t)
    | Trait (Value t) -> List.sum (module Int) t.fields ~f:(fun (_, t) -> get_size t)
    | ConstArray (Value { element_type; values = _ }) -> get_size element_type
    | Array (Value { element_type; values = _ }) -> get_size element_type
    | Ptr _
    | FunPtr _ ->
        8
    | Void -> 0
    | _ -> failwithf "todo: %s" (show t) ()

let rec get_offset t field =
    match t with
    | Struct (Value { name = _; fields })
    | Trait (Value { name = _; fields }) ->
        List.fold_until fields ~init:0
          ~f:(fun acc (name, t) ->
            if String.equal name field then
              Stop (Some acc)
            else
              Continue (acc + get_size t))
          ~finish:(fun _ -> None)
    | Ptr (Struct _ as s) -> get_offset s field
    | _ -> None

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
