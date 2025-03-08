open Core

(* TODO: aren't nodes and gvn basically the same thing? *)
type t = {
    dependencies : (Node.t, Node.t option Dynarray.t) Hashtbl.t;
    dependants : (Node.t, Node.t Hash_set.t) Hashtbl.t;
    nodes : Node.t Hash_set.t;
    start : Node.t;
    stop : Node.t;
    gvn : Node.t Hash_set.t;
  }

let create () =
    let start = Start_node.create () in
    let stop = Stop_node.create () in
    {
      dependencies = Hashtbl.create (module Node);
      dependants = Hashtbl.create (module Node);
      nodes = Hash_set.of_list (module Node) [ start; stop ];
      start;
      stop;
      gvn = Hash_set.of_list (module Node) [ start; stop ];
    }

let get_start g = g.start
let get_stop g = g.stop

let add_dependencies g node dependencies =
    Hash_set.add g.nodes node;
    Hashtbl.update g.dependencies node ~f:(function
      | None -> Dynarray.of_list dependencies
      | Some arr ->
          Dynarray.append_list arr dependencies;
          arr);
    List.iter dependencies ~f:(fun d ->
        match d with
        | None -> ()
        | Some d ->
            Hash_set.add g.nodes d;
            Hashtbl.update g.dependants d ~f:(function
              | None -> Hash_set.of_list (module Node) [ node ]
              | Some s ->
                  Hash_set.add s node;
                  s))

let set_dependency g node dep idx =
    assert (idx <> 0 || Node.is_ctrl (Option.value_exn dep));
    let arr = Hashtbl.find_exn g.dependencies node in
    let prev = Dynarray.get arr idx in
    (match prev with
    | None -> ()
    | Some n -> (
        let s = Hashtbl.find g.dependants n in
        match s with
        | None -> assert false
        | Some s -> Hash_set.remove s node));
    Dynarray.set arr idx dep;
    match dep with
    | None -> ()
    | Some dep ->
        Hash_set.add g.nodes dep;
        Hashtbl.update g.dependants dep ~f:(function
          | None -> Hash_set.of_list (module Node) [ node ]
          | Some s ->
              Hash_set.add s node;
              s)

let remove_dependency g ~node ~dep =
    Hashtbl.change g.dependencies node ~f:(function
      | None -> None
      | Some arr -> (
          match
            Dynarray.find_index
              (fun dep_opt ->
                match dep_opt with
                | None -> false
                | Some x -> Node.hard_equal dep x)
              arr
          with
          | None -> Some arr
          | Some idx ->
              Dynarray.set arr idx None;
              Some arr));

    Hashtbl.change g.dependants dep ~f:(function
      | None ->
          (* shouldn't happen *)
          if (not (Node.hard_equal dep g.start)) && not (Node.hard_equal dep g.stop) then (
            Hash_set.remove g.nodes dep;
            Hash_set.remove g.gvn dep);
          None
      | Some s ->
          Hash_set.remove s node;
          if
            Hash_set.is_empty s
            && (not (Node.hard_equal dep g.start))
            && not (Node.hard_equal dep g.stop)
          then (
            Hash_set.remove g.nodes dep;
            Hash_set.remove g.gvn dep;
            None)
          else
            Some s)

let iter g = Hash_set.iter g.nodes

let get_dependencies g n =
    match Hashtbl.find g.dependencies n with
    | None -> []
    | Some arr -> Dynarray.to_list arr

let get_dependency g n idx =
    match Hashtbl.find g.dependencies n with
    | None -> None
    | Some arr -> Dynarray.get arr idx

let get_dependants g n =
    match Hashtbl.find g.dependants n with
    | None -> []
    | Some s -> Hash_set.to_list s

let remove_node g n =
    let remove_dependencies () =
        match Hashtbl.find g.dependencies n with
        | None -> ()
        | Some deps ->
            Dynarray.iter
              (fun dep ->
                match dep with
                | None -> ()
                | Some dep -> remove_dependency g ~node:n ~dep)
              deps
    in
    match Hashtbl.find g.dependants n with
    | None ->
        remove_dependencies ();
        Hash_set.remove g.nodes n;
        Hash_set.remove g.gvn n
    | Some s when Hash_set.is_empty s ->
        Hash_set.remove g.nodes n;
        Hash_set.remove g.gvn n;
        Hashtbl.remove g.dependants n;
        remove_dependencies ()
    | Some _ -> Printf.printf "Couldn't remove node %s because it has dependants\n" (Node.show n)

let replace_node_with g base new_node =
    (match Hashtbl.find g.dependants base with
    | None -> ()
    | Some s ->
        Hash_set.to_list s
        |> List.iter ~f:(fun n ->
               match Hashtbl.find g.dependencies n with
               | None -> assert false
               | Some arr ->
                   let idx =
                       Dynarray.find_index
                         (function
                           | None -> false
                           | Some x -> Node.hard_equal base x)
                         arr
                       |> Option.value_exn
                   in
                   set_dependency g n (Some new_node) idx));
    remove_node g base

let set_stop_ctrl g ctrl = add_dependencies g g.stop [ Some ctrl ]
let get_num_nodes g = Hash_set.length g.nodes

let finalize_node g n =
    let n_deps = get_dependencies g n in
    match Hash_set.find g.gvn ~f:(fun x -> Node.is_same n n_deps x (get_dependencies g x)) with
    | None ->
        Hash_set.add g.gvn n;
        n
    | Some old_n ->
        if not (Node.hard_equal n old_n) then
          remove_node g n;
        old_n
