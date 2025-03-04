open Core

let gvn = Hash_set.create (module Node)

let finalize g n =
    let n_deps = Graph.get_dependencies g n in
    match Hash_set.find gvn ~f:(fun x -> Node.is_same n n_deps x (Graph.get_dependencies g x)) with
    | None ->
        Hash_set.add gvn n;
        n
    | Some old_n ->
        Graph.remove_node g n;
        old_n
