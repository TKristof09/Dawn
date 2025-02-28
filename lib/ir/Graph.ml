open Core

type t = {
    dependencies : (Node.t, Node.t list) Hashtbl.t;
    dependants : (Node.t, Node.t Hash_set.t) Hashtbl.t;
    nodes : Node.t Hash_set.t;
    start : Node.t;
  }

let create () =
    {
      dependencies = Hashtbl.create (module Node);
      dependants = Hashtbl.create (module Node);
      nodes = Hash_set.create (module Node);
      start = Start_node.create ();
    }

let get_start g = g.start

let add_dependencies g node dependencies =
    Hash_set.add g.nodes node;
    Hashtbl.update g.dependencies node ~f:(function
      | None -> dependencies
      | Some l -> dependencies @ l);
    List.iter dependencies ~f:(fun d ->
        Hash_set.add g.nodes d;
        Hashtbl.update g.dependants d ~f:(function
          | None -> Hash_set.of_list (module Node) [ node ]
          | Some s ->
              Hash_set.add s node;
              s))

let remove_dependency g ~node ~dep =
    Hashtbl.change g.dependencies node ~f:(function
      | None -> None
      | Some l ->
          if List.equal Node.equal l [ dep ] then
            None
          else
            Some (List.filter l ~f:(fun n -> not (Node.equal n dep))));
    Hashtbl.change g.dependants dep ~f:(function
      | None ->
          (* shouldn't happen *)
          Hash_set.remove g.nodes dep;
          None
      | Some s ->
          Hash_set.remove s node;
          if Hash_set.is_empty s then (
            Hash_set.remove g.nodes dep;
            None)
          else
            Some s)

let iter g = Hash_set.iter g.nodes

let get_dependencies g n =
    match Hashtbl.find g.dependencies n with
    | None -> []
    | Some l -> l

let get_dependants g n =
    match Hashtbl.find g.dependants n with
    | None -> []
    | Some s -> Hash_set.to_list s

let remove_node g n =
    let remove_dependencies () =
        match Hashtbl.find g.dependencies n with
        | None -> ()
        | Some deps -> List.iter deps ~f:(fun dep -> remove_dependency g ~node:n ~dep)
    in
    match Hashtbl.find g.dependants n with
    | None ->
        remove_dependencies ();
        Hash_set.remove g.nodes n
    | Some s when Hash_set.is_empty s ->
        Hash_set.remove g.nodes n;
        Hashtbl.remove g.dependants n;
        remove_dependencies ()
    | Some _ -> ()
