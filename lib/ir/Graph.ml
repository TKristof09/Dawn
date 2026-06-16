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
  val readonly : 'q t -> readonly t
  val add_node : readwrite t -> ('a, 't) N.t -> 'a -> unit
  val set_node_inputs : readwrite t -> ('a, 't) N.t -> 'a -> unit
  val set_ctrl : readwrite t -> ('a, 'ta) N.t -> ('b, 'tb) N.t -> unit
  val unlink_ctrl : readwrite t -> ('a, 'ta) N.t -> unit
  val remove_node : readwrite t -> ('a, 't) N.t -> unit
  val replace_node_with : readwrite t -> from:('a, 't) N.t -> to_:('a, 't) N.t -> unit
  val replace_node_with_unsafe : readwrite t -> from:N.any -> to_:N.any -> unit
  val get_start : 'q t -> N.any
  val get_stop : 'q t -> N.any
  val get_ctrl : 'q t -> ('a, 't) N.t -> N.any option
  val get_ctrl_exn : 'q t -> ('a, 't) N.t -> N.any
  val get_dependencies : 'q t -> ('a, 't) N.t -> 'a option
  val get_dependencies_exn : 'q t -> ('a, 't) N.t -> 'a
  val get_dependencies_list : 'q t -> ('a, 't) N.t -> N.any option list
  val get_dependants : 'q t -> ('a, 't) N.t -> N.any list

  val replace_input :
    readwrite t -> node:('a, 'ta) N.t -> from:('b, 'tb) N.t -> to_:('b, 'tb) N.t -> unit

  val replace_input_unsafe : readwrite t -> node:('a, 'ta) N.t -> from:N.any -> to_:N.any -> unit

  val partition :
    'q t ->
    f:(N.any -> int) ->
    get_start:(N.any -> bool) ->
    get_stop:(N.any -> bool) ->
    readwrite t list

  val mem : 'q t -> N.any -> bool
  val iter : 'q t -> f:(N.any -> unit) -> unit
  val fold : 'q t -> init:'c -> f:('c -> N.any -> 'c) -> 'c
  val find : 'q t -> f:(N.any -> bool) -> N.any option
  val find_map : 'q t -> f:(N.any -> 'a option) -> 'a option
  val get_num_nodes : 'q t -> int
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
      dependencies : (N.any, N.any option * entry) Hashtbl.t;
      dependants : (N.any, N.any list) Hashtbl.t;
      start : N.any;
      stop : N.any;
    }

  module HashableNode = struct
    type t = N.any

    let hash (N.AnyNode n) = Int.hash (N.id n)
    let equal (N.AnyNode a) (N.AnyNode b) = N.id a = N.id b
    let compare (N.AnyNode a) (N.AnyNode b) = Int.compare (N.id a) (N.id b)
    let sexp_of_t (N.AnyNode n) = Sexp.Atom (Printf.sprintf "Node #%d" (N.id n))
  end

  let create ~start ~stop =
      {
        dependencies = Hashtbl.create (module HashableNode);
        dependants = Hashtbl.create (module HashableNode);
        start;
        stop;
      }

  let readonly t = (t :> readonly t)

  let add_aux t n l =
      l
      |> List.filter_opt
      |> List.iter ~f:(fun (N.AnyNode n') ->
          Hashtbl.add_multi t.dependants ~key:(AnyNode n') ~data:(N.AnyNode n))

  let add_node t n inputs =
      let l = N.list_of_inputs n inputs in
      let ok = Hashtbl.add t.dependencies ~key:(AnyNode n) ~data:(None, Entry (n, l)) in
      match ok with
      | `Duplicate -> failwith "Node already part of graph"
      | `Ok -> add_aux t n l

  let get_start t = t.start
  let get_stop t = t.stop

  let get_ctrl t n =
      match Hashtbl.find t.dependencies (AnyNode n) with
      | None -> None
      | Some (ctrl, _) -> ctrl

  let get_ctrl_exn t n = get_ctrl t n |> Option.value_exn

  let get_dependencies : type q a tag. q t -> (a, tag) N.t -> a option =
     fun t n ->
      match Hashtbl.find t.dependencies (AnyNode n) with
      | None -> None
      | Some (_, Entry (n', e)) -> (
          match N.type_eq n n' with
          | Some (Type.Equal, Type.Equal) -> Some (N.inputs_of_list n e)
          | None -> None)

  let get_dependencies_exn : type q a tag. q t -> (a, tag) N.t -> a =
     fun t n -> get_dependencies t n |> Option.value_exn

  let get_dependencies_list t n =
      match Hashtbl.find t.dependencies (AnyNode n) with
      | None -> []
      | Some (_, Entry (_, e)) -> e

  let get_dependants g n = Hashtbl.find g.dependants (AnyNode n) |> Option.value ~default:[]

  let rec set_node_inputs t n inputs =
      match Hashtbl.find t.dependencies (AnyNode n) with
      | None -> failwith "Node not part of graph"
      | Some (ctrl, Entry (ne, old_inputs)) ->
          (* remove the edges old_input -> n *)
          List.iter old_inputs ~f:(function
            | None -> ()
            | Some (N.AnyNode old) ->
                let dependants = Hashtbl.find_exn t.dependants (AnyNode old) in
                let new_dependants =
                    List.filter dependants ~f:(fun (AnyNode n') -> N.id n' <> N.id n)
                in
                if List.is_empty new_dependants then
                  remove_node t old
                else
                  Hashtbl.set t.dependants ~key:(AnyNode old) ~data:new_dependants);
          (* add edges input -> n *)
          let l = N.list_of_inputs n inputs in
          Hashtbl.set t.dependencies ~key:(AnyNode n) ~data:(ctrl, Entry (n, l));
          add_aux t n l

  and remove_node : type a tag. readwrite t -> (a, tag) N.t -> unit =
     fun t n ->
      match Hashtbl.find_and_remove t.dependencies (AnyNode n) with
      | None -> ()
      | Some (ctrl, Entry (ne, inps)) ->
          (* remove n from the dependants of all its inputs (ctrl+inputs list) *)
          ctrl :: inps
          |> List.filter_opt
          |> List.iter ~f:(fun (N.AnyNode n') ->
              let dependants = Hashtbl.find_exn t.dependants (AnyNode n') in
              let new_dependants =
                  List.filter dependants ~f:(fun (AnyNode n'') -> N.id n'' <> N.id n)
              in
              if List.is_empty new_dependants then
                remove_node t n'
              else
                Hashtbl.set t.dependants ~key:(AnyNode n') ~data:new_dependants);
          (* remove n from all its dependants' dependencis (both as ctrl dependency and as normal dependency *)
          Hashtbl.find t.dependants (AnyNode n)
          |> Option.value ~default:[]
          |> List.iter ~f:(fun (AnyNode n') ->
              let ctrl', Entry (ne, e) = Hashtbl.find_exn t.dependencies (AnyNode n') in
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
              Hashtbl.set t.dependencies ~key:(AnyNode n') ~data:(new_ctrl', new_entry))

  let replace_input_unsafe g ~node ~from:(N.AnyNode from) ~to_ =
      match Hashtbl.find g.dependencies (N.AnyNode node) with
      | None -> failwith "Node not part of graph"
      | Some (ctrl, Entry (ne, le)) ->
          (* replace "from" with "to_" in node's dependencies *)
          (let new_le =
               List.map le ~f:(function
                 | None -> None
                 | Some (N.AnyNode n') as o -> if N.id n' = N.id from then Some to_ else o)
           in
           Hashtbl.set g.dependencies ~key:(N.AnyNode node) ~data:(ctrl, Entry (ne, new_le));
           (* remove "node" from the dependants of "from" *)
           match Hashtbl.find g.dependants (AnyNode from) with
           | None -> assert false
           | Some l ->
               let new_l = List.filter l ~f:(fun (AnyNode n') -> N.id n' <> N.id node) in
               if List.is_empty new_l then
                 Hashtbl.remove g.dependants (AnyNode from)
               else
                 Hashtbl.set g.dependants ~key:(AnyNode from) ~data:new_l);
          (* add "node" to "to_"'s dependants *)
          Hashtbl.add_multi g.dependants ~key:to_ ~data:(AnyNode node)

  let replace_input : type a ta b tb.
      readwrite t -> node:(a, ta) N.t -> from:(b, tb) N.t -> to_:(b, tb) N.t -> unit =
     fun g ~node ~from ~to_ -> replace_input_unsafe g ~node ~from:(AnyNode from) ~to_:(AnyNode to_)

  let replace_node_with_unsafe g ~from:(N.AnyNode from) ~to_ =
      let users = get_dependants g from in
      List.iter users ~f:(fun (N.AnyNode user) ->
          replace_input_unsafe g ~node:user ~from:(AnyNode from) ~to_);
      remove_node g from

  let replace_node_with : type a b. readwrite t -> from:(a, b) N.t -> to_:(a, b) N.t -> unit =
     fun g ~from ~to_ -> replace_node_with_unsafe g ~from:(N.AnyNode from) ~to_:(N.AnyNode to_)

  let set_ctrl_aux t n ctrl =
      match Hashtbl.find t.dependencies (AnyNode n) with
      | None -> failwith "Node not part of graph"
      | Some (old_ctrl, e) ->
          (* remove the edge old_ctrl -> n *)
          (match old_ctrl with
          | None -> ()
          | Some (N.AnyNode old_ctrl) ->
              let dependants = Hashtbl.find_exn t.dependants (AnyNode old_ctrl) in
              let new_dependants =
                  List.filter dependants ~f:(fun (AnyNode n') -> N.id n' <> N.id n)
              in
              if List.is_empty new_dependants then
                remove_node t old_ctrl
              else
                Hashtbl.set t.dependants ~key:(AnyNode old_ctrl) ~data:new_dependants);
          (* add edge ctrl -> n *)
          Hashtbl.set t.dependencies ~key:(AnyNode n) ~data:(ctrl, e);
          add_aux t n [ ctrl ]

  let set_ctrl t n ctrl = set_ctrl_aux t n (Some (N.AnyNode ctrl))
  let unlink_ctrl t n = set_ctrl_aux t n None
  let mem g n = Hashtbl.mem g.dependants n
  let iter g ~f = Hashtbl.iter_keys g.dependants ~f
  let fold t ~init ~f = Hashtbl.fold t.dependants ~init ~f:(fun ~key ~data:_ acc -> f acc key)

  let find g ~f =
      With_return.with_return (fun r ->
          Hashtbl.iter_keys g.dependants ~f:(fun key -> if f key then r.return (Some key));
          None)

  let find_map g ~f =
      With_return.with_return (fun r ->
          Hashtbl.iter_keys g.dependants ~f:(fun key ->
              match f key with
              | None -> ()
              | Some _ as o -> r.return o);
          None)

  let get_num_nodes g = Hashtbl.length g.dependants

  let partition g ~f ~get_start ~get_stop =
      let partitions = Hashtbl.create (module Int) in
      Hashtbl.iter_keys g.dependants ~f:(fun node ->
          let key = f node in
          let s =
              Hashtbl.find_or_add partitions key ~default:(fun _ ->
                  Hash_set.create (module HashableNode))
          in
          Hash_set.add s node);

      Hashtbl.to_alist partitions
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      |> List.map ~f:(fun (_, partition_nodes) ->
          let new_deps = Hashtbl.create (module HashableNode) in
          let new_dependants = Hashtbl.create (module HashableNode) in

          (* Copy edges that have both endpoints in this partition *)
          Hash_set.iter partition_nodes ~f:(fun node ->
              (* Handle dependencies *)
              (match Hashtbl.find g.dependencies node with
              | Some (ctrl, Entry (ne, le)) ->
                  let filtered_deps =
                      List.map le ~f:(function
                        | None -> None
                        | Some dep -> if Hash_set.mem partition_nodes dep then Some dep else None)
                  in
                  let new_ctrl =
                      Option.bind ctrl ~f:(fun ctrl ->
                          if Hash_set.mem partition_nodes ctrl then Some ctrl else None)
                  in
                  Hashtbl.set new_deps ~key:node ~data:(new_ctrl, Entry (ne, filtered_deps))
              | None -> ());

              (* Handle dependants *)
              match Hashtbl.find g.dependants node with
              | Some dependants ->
                  let filtered_dependants =
                      List.filter dependants ~f:(Hash_set.mem partition_nodes)
                  in
                  Hashtbl.set new_dependants ~key:node ~data:filtered_dependants
              | None -> ());

          let start = Hash_set.find partition_nodes ~f:get_start |> Option.value_exn in
          let stop = Hash_set.find partition_nodes ~f:get_stop |> Option.value_exn in
          { dependencies = new_deps; dependants = new_dependants; start; stop })
end
