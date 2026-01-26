open Sexplib.Std

type 'a lattice =
    | Top
    | Value of 'a
    | Bottom
[@@deriving sexp_of]

let pp_lattice pp_a fmt = function
    | Top -> Format.fprintf fmt "Top"
    | Bottom -> Format.fprintf fmt "Bottom"
    | Value v -> pp_a fmt v

type node_type =
    | TOP
    | Integer of int lattice
    | Tuple of node_type list lattice
    | Control
    | BOTTOM
[@@deriving show { with_path = false }, sexp_of]

let rec meet t t' =
    let meet_aux t t' =
        match (t, t') with
        | Bottom, _ -> Bottom
        | _, Bottom -> Bottom
        | _, Top -> t
        | Top, _ -> t'
        | Value x, Value x' when x = x' -> Value x
        | Value _, Value _ -> Bottom
    and meet_tuple t t' =
        match (t, t') with
        | Bottom, _ -> Bottom
        | _, Bottom -> Bottom
        | _, Top -> t
        | Top, _ -> t'
        | Value x, Value x' -> (
            match Core.List.map2 x x' ~f:meet with
            | Core.List.Or_unequal_lengths.Ok l -> Value l
            | Core.List.Or_unequal_lengths.Unequal_lengths -> Bottom)
    in
    match (t, t') with
    | Integer t, Integer t' -> Integer (meet_aux t t')
    | Tuple t, Tuple t' -> Tuple (meet_tuple t t')
    | Control, Control -> Control
    | BOTTOM, _ -> BOTTOM
    | _, BOTTOM -> BOTTOM
    | TOP, _ -> TOP
    | _, TOP -> TOP
    | _, _ -> BOTTOM

let rec join t t' =
    let join_aux t t' =
        match (t, t') with
        | Bottom, _ -> t'
        | _, Bottom -> t
        | _, Top
        | Top, _ ->
            Top
        | Value x, Value x' when x = x' -> Value x
        | Value _, Value _ -> Top
    and join_tuple t t' =
        match (t, t') with
        | Bottom, _ -> t'
        | _, Bottom -> t
        | _, Top 
        | Top, _ -> Top
        | Value x, Value x' -> (
            match Core.List.map2 x x' ~f:join with
            | Core.List.Or_unequal_lengths.Ok l -> Value l
            | Core.List.Or_unequal_lengths.Unequal_lengths -> Bottom)
    in
    match (t, t') with
    | Integer t, Integer t' -> Integer (join_aux t t')
    | Tuple t, Tuple t' -> Tuple (join_tuple t t')
    | Control, Control -> Control
    | BOTTOM, _ -> t'
    | _, BOTTOM -> t
    | TOP, _
    | _, TOP ->
        TOP
    | _, _ -> TOP

let is_constant t =
    let is_constant_lattice = function
        | Top
        | Bottom ->
            false
        | Value _ -> true
    in
    match t with
    | TOP
    | BOTTOM
    | Control ->
        false
    | Integer x -> is_constant_lattice x
    | Tuple x -> is_constant_lattice x
