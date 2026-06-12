open Core

module type NODE = sig
  type ('a, 'tag) t
  type any = AnyNode : ('a, 'tag) t -> any

  val id : ('a, 'tag) t -> int
  val list_of_inputs : ('a, 'tag) t -> 'a -> any option list
  val inputs_of_list : ('a, 'tag) t -> any option list -> 'a
  val type_eq : ('a, 'taga) t -> ('b, 'tagb) t -> (('a, 'b) Type.eq * ('taga, 'tagb) Type.eq) option
end

module type S = sig
  module N : NODE

  type readonly
  type readwrite
  type 'q t

  val create : start:N.any -> stop:N.any -> readwrite t
  val add_node : readwrite t -> ('a, 't) N.t -> 'a -> unit
  val set_node_inputs : readwrite t -> ('a, 't) N.t -> 'a -> unit
  val set_ctrl : readwrite t -> ('a, 'ta) N.t -> ('b, 'tb) N.t -> unit
  val remove_node : readwrite t -> ('a, 't) N.t -> unit
  val get_start : 'q t -> N.any
  val get_stop : 'q t -> N.any
  val get_ctrl : 'q t -> ('a, 't) N.t -> N.any option
  val get_ctrl_exn : 'q t -> ('a, 't) N.t -> N.any
  val get_dependencies : 'q t -> ('a, 't) N.t -> 'a option
  val get_dependencies_exn : 'q t -> ('a, 't) N.t -> 'a
  val get_dependencies_list : 'q t -> ('a, 't) N.t -> N.any option list
  val get_dependants : 'q t -> ('a, 't) N.t -> N.any list
  val readonly : 'q t -> readonly t
end

module Make (N : NODE) : S with module N := N = struct
  type readonly
  type readwrite
  type entry = Entry : ('a, 't) N.t * N.any option list -> entry

  (* invariants:
      - dependencies and dependants kept in sync: for all (n, [inp1, inp2...]) in dependencies, n is in dependants[inp1] 
      - if no dependants then remove from graph: for all (n, users) in dependants, len(users) > 0
  *)
  type 'q t = {
      dependencies : (int, N.any option * entry) Hashtbl.t;
      dependants : (int, N.any list) Hashtbl.t;
      start : N.any;
      stop : N.any;
    }

  let create ~start ~stop =
      {
        dependencies = Hashtbl.create (module Int);
        dependants = Hashtbl.create (module Int);
        start;
        stop;
      }

  let readonly t = (t :> readonly t)

  let add_aux t n l =
      l
      |> List.filter_opt
      |> List.iter ~f:(fun (N.AnyNode n') ->
          Hashtbl.add_multi t.dependants ~key:(N.id n') ~data:(N.AnyNode n))

  let add_node t n inputs =
      let l = N.list_of_inputs n inputs in
      let ok = Hashtbl.add t.dependencies ~key:(N.id n) ~data:(None, Entry (n, l)) in
      match ok with
      | `Duplicate -> failwith "Node already part of graph"
      | `Ok -> add_aux t n l

  let rec set_node_inputs t n inputs =
      match Hashtbl.find t.dependencies (N.id n) with
      | None -> failwith "Node not part of graph"
      | Some (ctrl, Entry (ne, old_inputs)) ->
          (* remove the edges old_input -> n *)
          List.iter old_inputs ~f:(function
            | None -> ()
            | Some (N.AnyNode old) ->
                let dependants = Hashtbl.find_exn t.dependants (N.id old) in
                let new_dependants =
                    List.filter dependants ~f:(fun (AnyNode n') -> N.id n' <> N.id n)
                in
                if List.is_empty new_dependants then
                  remove_node t old
                else
                  Hashtbl.set t.dependants ~key:(N.id old) ~data:new_dependants);
          (* add edges input -> n *)
          let l = N.list_of_inputs n inputs in
          Hashtbl.set t.dependencies ~key:(N.id n) ~data:(ctrl, Entry (n, l));
          add_aux t n l

  and remove_node : type a tag. readwrite t -> (a, tag) N.t -> unit =
     fun t n ->
      match Hashtbl.find_and_remove t.dependencies (N.id n) with
      | None -> ()
      | Some (ctrl, Entry (ne, inps)) ->
          (* remove n from the dependants of all its inputs (ctrl+inputs list) *)
          ctrl :: inps
          |> List.filter_opt
          |> List.iter ~f:(fun (N.AnyNode n') ->
              let dependants = Hashtbl.find_exn t.dependants (N.id n') in
              let new_dependants =
                  List.filter dependants ~f:(fun (AnyNode n'') -> N.id n'' <> N.id n)
              in
              if List.is_empty new_dependants then
                remove_node t n'
              else
                Hashtbl.set t.dependants ~key:(N.id n') ~data:new_dependants);
          (* remove n from all its dependants' dependencis (both as ctrl dependency and as normal dependency *)
          Hashtbl.find t.dependants (N.id n)
          |> Option.value ~default:[]
          |> List.iter ~f:(fun (AnyNode n') ->
              let ctrl', Entry (ne, e) = Hashtbl.find_exn t.dependencies (N.id n') in
              let new_ctrl' =
                  match (ctrl, ctrl') with
                  | Some (AnyNode ctrl), Some (AnyNode ctrl') when N.id ctrl = N.id ctrl' -> None
                  | _, _ -> ctrl'
              in
              let new_entry =
                  Entry
                    ( ne,
                      List.map e ~f:(function
                        | Some (AnyNode n'') when N.id n'' = N.id n -> None
                        | o -> o) )
              in
              Hashtbl.set t.dependencies ~key:(N.id n') ~data:(new_ctrl', new_entry))

  let set_ctrl t n ctrl =
      match Hashtbl.find t.dependencies (N.id n) with
      | None -> failwith "Node not part of graph"
      | Some (old_ctrl, e) ->
          (* remove the edge old_ctrl -> n *)
          (match old_ctrl with
          | None -> ()
          | Some (N.AnyNode old_ctrl) ->
              let dependants = Hashtbl.find_exn t.dependants (N.id old_ctrl) in
              let new_dependants =
                  List.filter dependants ~f:(fun (AnyNode n') -> N.id n' <> N.id n)
              in
              if List.is_empty new_dependants then
                remove_node t old_ctrl
              else
                Hashtbl.set t.dependants ~key:(N.id old_ctrl) ~data:new_dependants);
          (* add edge ctrl -> n *)
          Hashtbl.set t.dependencies ~key:(N.id n) ~data:(Some (N.AnyNode ctrl), e);
          add_aux t n [ Some (N.AnyNode ctrl) ]

  let get_start t = t.start
  let get_stop t = t.stop

  let get_ctrl t n =
      match Hashtbl.find t.dependencies (N.id n) with
      | None -> None
      | Some (ctrl, _) -> ctrl

  let get_ctrl_exn t n = get_ctrl t n |> Option.value_exn

  let get_dependencies : type q a tag. q t -> (a, tag) N.t -> a option =
     fun t n ->
      match Hashtbl.find t.dependencies (N.id n) with
      | None -> None
      | Some (_, Entry (n', e)) -> (
          match N.type_eq n n' with
          | Some (Type.Equal, Type.Equal) -> Some (N.inputs_of_list n e)
          | None -> None)

  let get_dependencies_exn : type q a tag. q t -> (a, tag) N.t -> a =
     fun t n -> get_dependencies t n |> Option.value_exn

  let get_dependencies_list t n =
      match Hashtbl.find t.dependencies (N.id n) with
      | None -> []
      | Some (_, Entry (_, e)) -> e

  let get_dependants g n = Hashtbl.find g.dependants (N.id n) |> Option.value ~default:[]
end

module type GraphNode = sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val semantic_equal : t -> t option list -> t -> t option list -> bool
  val hash : t -> int
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t

  (* TODO: This is for scope nodes so they don't get removed for not having any dependants. It feels very hacky though so i don't like it *)
  val is_persistent : t -> bool
end

type readonly
type readwrite

(* TODO: aren't nodes and gvn basically the same thing? *)
type ('n, 'b) t = {
    dependencies : ('n, 'n option Dynarray.t) Hashtbl.t;
    dependants : ('n, 'n Dynarray.t) Hashtbl.t;
    nodes : 'n Hash_set.t;
    start : 'n;
    stop : 'n;
    gvn : 'n Hash_set.t;
    node_module : (module GraphNode with type t = 'n);
  }

let readonly t = (t :> ('a, readonly) t)

let remove_arr_elt arr elt ~eq =
    match Dynarray.find_index (eq elt) arr with
    | None -> assert false
    | Some idx ->
        let last = Dynarray.pop_last arr in
        if idx < Dynarray.length arr then Dynarray.set arr idx last

let check (type n) g =
    let module Node = (val g.node_module : GraphNode with type t = n) in
    let debug_check () =
        let dependencies_keys_match =
            Hash_set.of_hashtbl_keys g.dependencies |> Hash_set.equal g.nodes
        in
        if not dependencies_keys_match then
          failwith
          @@ Printf.sprintf "Depedencies keys not match: +dep: %s ------ +nodes: %s"
               ([%derive.show: Node.t list]
                  (Hash_set.diff (Hash_set.of_hashtbl_keys g.dependencies) g.nodes
                  |> Hash_set.to_list))
               ([%derive.show: Node.t list]
                  (Hash_set.diff g.nodes (Hash_set.of_hashtbl_keys g.dependencies)
                  |> Hash_set.to_list));
        let dependants_keys_match =
            Hash_set.of_hashtbl_keys g.dependants |> Hash_set.equal g.nodes
        in
        if not dependants_keys_match then
          failwith
          @@ Printf.sprintf "Dependants keys not match: +dep: %s ------ +nodes: %s"
               ([%derive.show: Node.t list]
                  (Hash_set.diff (Hash_set.of_hashtbl_keys g.dependants) g.nodes |> Hash_set.to_list))
               ([%derive.show: Node.t list]
                  (Hash_set.diff g.nodes (Hash_set.of_hashtbl_keys g.dependants) |> Hash_set.to_list));
        Hashtbl.iteri g.dependencies ~f:(fun ~key ~data ->
            Dynarray.iter
              (function
                | None -> ()
                | Some n -> (
                    match Hashtbl.find g.dependants n with
                    | None ->
                        failwith
                        @@ Printf.sprintf
                             "Depedencies not in sync with dependants: couldn't find dependants of \
                              dependency %s of %s"
                             (Node.show n) (Node.show key)
                    | Some arr ->
                        if not (Dynarray.mem key arr) then
                          failwith
                          @@ Printf.sprintf
                               "Depedencies not in sync with dependants: dependency %s of %s"
                               (Node.show n) (Node.show key)))
              data);

        Hashtbl.iteri g.dependants ~f:(fun ~key ~data ->
            Dynarray.iter
              (fun n ->
                match Hashtbl.find g.dependencies n with
                | None ->
                    failwith
                    @@ Printf.sprintf
                         "Dependants not in sync with depedencies: couldn't find depedencies of \
                          dependant %s of %s"
                         (Node.show n) (Node.show key)
                | Some arr ->
                    if not (Dynarray.mem (Some key) arr) then
                      failwith
                      @@ Printf.sprintf
                           "Dependants not in sync with depedencies: dependant %s of %s"
                           (Node.show n) (Node.show key))
              data)
    in
    match Sys.getenv "GRAPH_CHECK" with
    | None -> ()
    | Some _ -> debug_check ()

let add_node (g : ('a, readwrite) t) n =
    if Hash_set.mem g.nodes n then
      ()
    else (
      check g;
      assert (not (Hashtbl.mem g.dependants n));
      assert (not (Hashtbl.mem g.dependencies n));
      Hash_set.add g.nodes n;
      Hashtbl.set g.dependencies ~key:n ~data:(Dynarray.create ());
      Hashtbl.set g.dependants ~key:n ~data:(Dynarray.create ());
      check g)

let create (type a) (module M : GraphNode with type t = a) (start : a) (stop : a) =
    let g : (a, readwrite) t =
        {
          dependencies = Hashtbl.create (module M);
          dependants = Hashtbl.create (module M);
          nodes = Hash_set.create (module M);
          start;
          stop;
          gvn = Hash_set.of_list (module M) [ start; stop ];
          node_module = (module M);
        }
    in
    add_node g start;
    add_node g stop;
    check g;
    g

let get_start g = g.start
let get_stop g = g.stop

let set_dependency (type a) g node dep idx =
    (* TODO: shouldn't this clean up the old dep if it no longer has any uses? like remove_dependency *)
    let module Node = (val g.node_module : GraphNode with type t = a) in
    (* assert (idx <> 0 || Option.value_map dep ~default:true ~f:Node.is_ctrl || Node.is_scope node); *)
    check g;
    let arr = Hashtbl.find_exn g.dependencies node in
    let prev = Dynarray.get arr idx in
    (match prev with
    | None -> ()
    | Some n -> (
        let s = Hashtbl.find g.dependants n in
        match s with
        | None -> assert false
        | Some s ->
            remove_arr_elt s node ~eq:Node.equal;
            Dynarray.set arr idx None));

    Option.iter dep ~f:(add_node g);
    Dynarray.set arr idx dep;
    (match dep with
    | None -> ()
    | Some dep ->
        Hashtbl.update g.dependants dep ~f:(function
          | None -> assert false
          | Some s ->
              Dynarray.add_last s node;
              s));

    check g

let add_dependencies g node dependencies =
    check g;
    add_node g node;
    let arr = Hashtbl.find_exn g.dependencies node in
    List.iter dependencies ~f:(fun d ->
        let idx = Dynarray.length arr in
        Dynarray.add_last arr None;
        set_dependency g node d idx);
    check g

let node_is_removable (type a) g n =
    let module Node = (val g.node_module : GraphNode with type t = a) in
    not (Node.equal n g.start || Node.equal n g.stop || Node.is_persistent n)

let rec remove_dependency : type a. (a, readwrite) t -> node:a -> dep:a -> unit =
   fun g ~node ~dep ->
    let module Node = (val g.node_module : GraphNode with type t = a) in
    check g;
    Hashtbl.change g.dependencies node ~f:(function
      | None -> None
      | Some arr -> (
          match Dynarray.find_index (Option.value_map ~default:false ~f:(Node.equal dep)) arr with
          | None -> Some arr
          | Some idx ->
              set_dependency g node None idx;
              Some arr));
    check g;

    (match Hashtbl.find g.dependants dep with
    | None ->
        (* shouldn't happen *)
        [%log.error "Remove depedency failed: Node: %a Dep: %a" Node.pp node Node.pp dep];
        assert false
    | Some arr ->
        if Dynarray.is_empty arr && node_is_removable g dep then
          remove_node g dep);
    check g

and remove_node : type a. (a, readwrite) t -> a -> unit =
   fun g n ->
    let module Node = (val g.node_module : GraphNode with type t = a) in
    check g;
    (match Hashtbl.find g.dependants n with
    | None -> failwithf "Node %s is not part of the graph" (Node.show n) ()
    | Some s when Dynarray.is_empty s ->
        (match Hashtbl.find g.dependencies n with
        | None -> ()
        | Some deps ->
            Dynarray.iter
              (fun dep ->
                match dep with
                | None -> ()
                | Some dep -> remove_dependency g ~node:n ~dep)
              deps);
        Hashtbl.remove g.dependants n;
        Hashtbl.remove g.dependencies n;
        Hash_set.remove g.nodes n;
        Hash_set.remove g.gvn n
    | Some _ ->
        [%log.warn
            "Couldn't remove node %a because it has dependants or is a persistent node\n" Node.pp n]);
    check g

let replace_node_with (type a) g base new_node =
    let module Node = (val g.node_module : GraphNode with type t = a) in
    check g;
    (match Hashtbl.find g.dependants base with
    | None -> ()
    | Some s ->
        Dynarray.copy s
        |> Dynarray.iter (fun n ->
            match Hashtbl.find g.dependencies n with
            | None -> assert false
            | Some arr ->
                let idx =
                    Dynarray.find_index (Option.value_map ~default:false ~f:(Node.equal base)) arr
                    |> Option.value_exn
                in
                set_dependency g n (Some new_node) idx));
    remove_node g base;
    check g

let set_stop_ctrl g ctrl =
    check g;
    add_dependencies g g.stop [ Some ctrl ];
    check g

let get_num_nodes g = Hash_set.length g.nodes
let mem g n = Hash_set.mem g.nodes n
let iter g = Hash_set.iter g.nodes
let fold g = Hash_set.fold g.nodes
let find g = Hash_set.find g.nodes

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
    | Some s -> Dynarray.to_list s

let finalize_node (type a) g n =
    let module Node = (val g.node_module : GraphNode with type t = a) in
    let n_deps = get_dependencies g n in
    match
      Hash_set.find g.gvn ~f:(fun x -> Node.semantic_equal n n_deps x (get_dependencies g x))
    with
    | None ->
        Hash_set.add g.gvn n;
        n
    | Some old_n ->
        if not (Node.equal n old_n) then
          remove_node g n;
        old_n

let cleanup (type a) g =
    let module Node = (val g.node_module : GraphNode with type t = a) in
    let dead_code = Hashtbl.filter g.dependants ~f:Dynarray.is_empty in
    Hashtbl.iter_keys dead_code ~f:(fun n -> if not (Node.equal n g.stop) then remove_node g n)

let partition (type a) (g : (a, 'b) t) ~(f : a -> int) ~(get_start : a -> bool)
    ~(get_stop : a -> bool) =
    let module Node = (val g.node_module : GraphNode with type t = a) in
    let partitions = Hashtbl.create (module Int) in
    Hash_set.iter g.nodes ~f:(fun node ->
        let key = f node in
        let s =
            Hashtbl.find_or_add partitions key ~default:(fun _ -> Hash_set.create (module Node))
        in
        Hash_set.add s node);

    Hashtbl.to_alist partitions
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.map ~f:(fun (_, partition_nodes) ->
        let new_deps = Hashtbl.create (module Node) in
        let new_dependants = Hashtbl.create (module Node) in

        (* Copy edges that have both endpoints in this partition *)
        Hash_set.iter partition_nodes ~f:(fun node ->
            (* Handle dependencies *)
            (match Hashtbl.find g.dependencies node with
            | Some deps ->
                let filtered_deps =
                    Dynarray.map
                      (function
                        | None -> None
                        | Some dep -> if Hash_set.mem partition_nodes dep then Some dep else None)
                      deps
                in
                Hashtbl.set new_deps ~key:node ~data:filtered_deps
            | None -> ());

            (* Handle dependants *)
            match Hashtbl.find g.dependants node with
            | Some dependants ->
                let filtered_dependants =
                    Dynarray.filter (Hash_set.mem partition_nodes) dependants
                in
                Hashtbl.set new_dependants ~key:node ~data:filtered_dependants
            | None -> ());

        let start = Hash_set.find partition_nodes ~f:get_start |> Option.value_exn in
        let stop = Hash_set.find partition_nodes ~f:get_stop |> Option.value_exn in
        {
          dependencies = new_deps;
          dependants = new_dependants;
          nodes = partition_nodes;
          start;
          stop;
          gvn = Hash_set.filter g.gvn ~f:(Hash_set.mem partition_nodes);
          node_module = g.node_module;
        })
