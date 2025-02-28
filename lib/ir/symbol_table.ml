open Core

type 'a t = {
    parent : 'a t option;
    symbols : (string, 'a) Hashtbl.t;
  }
[@@deriving sexp_of]

let create () = { parent = None; symbols = Hashtbl.create (module String) }

let show t f =
    let rec show_scopes t depth =
        let indent = String.make (depth * 2) ' ' in
        let current_scope =
            Hashtbl.fold t.symbols ~init:"" ~f:(fun ~key ~data acc ->
                acc ^ Printf.sprintf "%s%s : %s\n" indent key (f data))
        in
        match t.parent with
        | None -> current_scope
        | Some p ->
            let parent_scope = show_scopes p (depth + 1) in
            current_scope ^ Printf.sprintf "%sParent scope:\n%s" indent parent_scope
    in
    Printf.sprintf "Symbol table: {\n%s}" (show_scopes t 1)

let push t = { parent = Some t; symbols = Hashtbl.create (module String) }

let pop t =
    match t.parent with
    | None -> failwith "Can't pop top level symbol table"
    | Some p -> p

let rec find_symbol t s =
    match Hashtbl.find t.symbols s with
    | Some sym -> sym
    | None -> (
        match t.parent with
        | Some p -> find_symbol p s
        | None -> failwith "Symbol not found")

let rec reassign_symbol t sym value =
    if Hashtbl.mem t.symbols sym then
      Hashtbl.set t.symbols ~key:sym ~data:value
    else
      match t.parent with
      | Some p -> reassign_symbol p sym value
      | None -> failwith "Symbol not found"

let add_symbol t name value = Hashtbl.set t.symbols ~key:name ~data:value

let iter t f =
    let rec aux t d acc =
        let acc =
            Hashtbl.fold t.symbols ~init:acc ~f:(fun ~key ~data acc -> (key, Some data, d) :: acc)
        in
        match t.parent with
        | None -> acc
        | Some p -> aux p (d + 1) (("", None, d) :: acc)
    in
    let symbols = aux t 0 [] in
    match symbols with
    | [] -> ()
    | (_, _, max_depth) :: _ ->
        List.iter symbols ~f:(fun (key, data, depth) ->
            f ~name:key ~symbol:data ~depth:(max_depth - depth))

let iter_current_depth t f = Hashtbl.iteri t.symbols ~f:(fun ~key ~data -> f ~name:key ~symbol:data)

let rec merge this other diff_fn eq =
    (* we suppose the two symbol tables have the same depth, this should be the case when merging at the end of regions *)
    Hashtbl.merge_into ~src:other.symbols ~dst:this.symbols ~f:(fun ~key n_other n_this ->
        match n_this with
        | None -> Set_to n_other (* I don't think this can actually happen when merging regions *)
        | Some n_this when eq n_this n_other -> Set_to n_this
        | Some n_this -> Set_to (diff_fn ~name:key ~this:n_this ~other:n_other));
    match (this.parent, other.parent) with
    | None, None -> ()
    | _, None
    | None, _ ->
        assert false
    | Some p_this, Some p_other -> merge p_this p_other diff_fn eq
