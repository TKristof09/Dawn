open Core

type 'a sub_lattice =
    | Any
    | Value of 'a
    | All
[@@deriving sexp_of]

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
    params : node_type list;
    ret : node_type;
    (* cofinite sets to allow us to move up/down the lattice nicely without
       having to resort to having a finite number of functions. `Include
       means only the ids in the Int.Set.t, exclude means everything except
       the ids in the Int.Set.t *)
    fun_indices : [ `Include of Int.Set.t | `Exclude of Int.Set.t ]; [@printer pp_fun_indices]
  }
[@@deriving.show { with_paths = false }]

and node_type =
    | ANY
    | Integer of int sub_lattice
    | Tuple of node_type list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Memory
    | Control
    | ALL
[@@deriving show { with_path = false }, sexp_of]

let of_ast_type (ast_type : Ast.var_type) : node_type =
    (* FIXME actually implement *)
    match ast_type with
    | Ast.Type "int" -> Integer Any
    | _ -> assert false

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
                | List.Or_unequal_lengths.Ok l -> Value l
                | List.Or_unequal_lengths.Unequal_lengths -> All)
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
                | List.Or_unequal_lengths.Ok params -> Value { params; ret; fun_indices }
                | List.Or_unequal_lengths.Unequal_lengths -> All)
        in
        FunPtr l
    | Control, Control -> Control
    | Memory, Memory -> Memory
    | ALL, _
    | _, ALL ->
        ALL
    | ANY, _ -> t'
    | _, ANY -> t
    | Integer _, _
    | Tuple _, _
    | FunPtr _, _
    | Control, _
    | Memory, _ ->
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
                match Core.List.map2 x x' ~f:join with
                | Core.List.Or_unequal_lengths.Ok l -> Value l
                | Core.List.Or_unequal_lengths.Unequal_lengths -> Any)
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
                | List.Or_unequal_lengths.Ok params -> Value { params; ret; fun_indices }
                | List.Or_unequal_lengths.Unequal_lengths -> All)
        in
        FunPtr l
    | Control, Control -> Control
    | Memory, Memory -> Memory
    | ALL, _ -> t'
    | _, ALL -> t
    | ANY, _
    | _, ANY ->
        ANY
    | Integer _, _
    | Tuple _, _
    | FunPtr _, _
    | Control, _
    | Memory, _ ->
        ANY

let is_constant t =
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
    | Memory ->
        false
    | Integer x -> is_constant_lattice x
    | Tuple x -> is_constant_lattice x
    | FunPtr x -> is_constant_lattice x

let get_fun_idx t =
    match t with
    | FunPtr (Value ({ params = _; ret = _; fun_indices } as fun_ptr)) -> (
        match fun_indices with
        | `Include s when Set.length s = 1 -> Set.choose_exn s
        | `Include _
        | `Exclude _ ->
            failwithf "Function idx couldn't be determined %s" (show_fun_ptr fun_ptr) ())
    | _ -> assert false
