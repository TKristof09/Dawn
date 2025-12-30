open Core

let id_counter = ref 0

let next_id () =
    incr id_counter;
    !id_counter

type cmp = Eq [@@deriving show { with_path = false }, sexp_of]

type ideal =
    | Loop
    | CProj of int
    | Start
    | Stop
    | Region
    | Phi
[@@deriving show { with_path = false }, sexp_of]

type machine_node_kind =
    | Add
    | AddImm of int
    | Sub
    | SubImm of int
    | Cmp
    | CmpImm of int
    | Set of cmp
    | Jmp of cmp
    | Int
    | Mov
    | DProj of int
    (* nodes that have no machine equivalent *)
    | Ideal of ideal
[@@deriving show { with_path = false }, sexp_of]

type t = {
    id : int;
    kind : machine_node_kind;
    ir_node : Node.t; [@opaque]
    in_regs : t Graph.t -> t -> int -> Registers.Mask.t option;
    out_reg : t Graph.t -> t -> int -> Registers.Mask.t option;
  }
[@@deriving show { with_path = false }, sexp_of]

let equal n n' = n.id = n'.id
let compare n n' = Int.compare n.id n'.id
let hash n = Int.hash n.id

let is_cheap_to_clone n =
    match n.kind with
    | Int -> true
    | _ -> false

let is_control_node n =
    match n.kind with
    | Jmp _
    | Ideal Start
    | Ideal Stop
    | Ideal Loop
    | Ideal Region
    | Ideal (CProj _) ->
        true
    | Ideal Phi
    | Add
    | AddImm _
    | Sub
    | SubImm _
    | Cmp
    | CmpImm _
    | Set _
    | Int
    | Mov
    | DProj _ ->
        false

let is_blockhead n =
    match n.kind with
    | Ideal Start
    | Ideal (CProj _)
    | Ideal Region
    | Ideal Loop
    | Ideal Stop ->
        true
    | _ -> false

let is_two_address n =
    match n.kind with
    | Add
    | AddImm _
    | Sub
    | SubImm _ ->
        true
    | _ -> false

let no_regs = fun _ _ _ -> None

let get_in_reg_mask (kind : machine_node_kind) =
    match kind with
    | Add
    | Sub ->
        fun _ _ i ->
          assert (i <= 1);
          Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.general_r)
    | AddImm _
    | SubImm _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.general_w
    | Int -> no_regs
    | DProj _ -> no_regs
    | Cmp ->
        fun _ _ i ->
          assert (i <= 1);
          Some Registers.Mask.general_r
    | CmpImm _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.general_r
    | Set _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.flags
    | Jmp _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.flags
    | Mov ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.all
    | Ideal _ -> no_regs

and get_out_reg_mask (kind : machine_node_kind) =
    match kind with
    | Add
    | Sub
    | AddImm _
    | SubImm _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.general_w
    | Int ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.general_w
    | DProj i ->
        fun g n _ ->
          let in_node = Graph.get_dependants g n |> List.hd in
          Option.bind in_node ~f:(fun dep -> dep.out_reg g dep i)
    | Cmp
    | CmpImm _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.flags
    | Set _ ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.general_w
    | Mov ->
        fun _ _ i ->
          assert (i = 0);
          Some Registers.Mask.all
    | Jmp _ -> no_regs
    | Ideal _ -> no_regs

let rec of_data_node (g : Node.t Graph.t) (machine_g : t Graph.t) (kind : Node.data_kind)
    (n : Node.t) =
    match kind with
    | Add -> (
        let deps = Graph.get_dependencies g n in
        match
          List.find_map deps ~f:(fun n ->
              Option.bind n ~f:(fun n ->
                  match n.typ with
                  | Integer (Value _) -> Some n
                  | _ -> None))
        with
        | None ->
            let kind = Add in
            let in_regs = get_in_reg_mask kind
            and out_reg = get_out_reg_mask kind in
            let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
            Graph.add_dependencies machine_g node [];
            Graph.add_dependencies machine_g node
              (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
            node
        | Some cn ->
            let value =
                match cn.typ with
                | Integer (Value v) -> v
                | _ -> assert false (* already checked in the List.find call above *)
            in
            let kind = AddImm value in
            let in_regs = get_in_reg_mask kind
            and out_reg = get_out_reg_mask kind in
            let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
            Graph.add_dependencies machine_g node [];
            let deps =
                List.filter deps ~f:(fun n ->
                    match n with
                    | None -> true
                    | Some n -> not (Node.equal cn n))
            in
            Graph.add_dependencies machine_g node
              (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
            node)
    | Sub -> (
        let deps = Graph.get_dependencies g n in
        match List.nth_exn deps 2 with
        | None -> assert false
        | Some dep -> (
            match dep.typ with
            | Integer (Value v) ->
                let kind = SubImm v in
                let in_regs = get_in_reg_mask kind
                and out_reg = get_out_reg_mask kind in
                let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
                Graph.add_dependencies machine_g node [];
                let deps =
                    List.filter deps ~f:(fun n ->
                        match n with
                        | None -> true
                        | Some n -> not (Node.equal dep n))
                in
                Graph.add_dependencies machine_g node
                  (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
                node
            | _ ->
                let kind = Sub in
                let in_regs = get_in_reg_mask kind
                and out_reg = get_out_reg_mask kind in
                let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
                Graph.add_dependencies machine_g node [];
                Graph.add_dependencies machine_g node
                  (List.map deps ~f:(Option.map ~f:(convert_node g machine_g)));
                node))
    | Constant ->
        let kind = Int in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [ Some (Graph.get_start machine_g) ];
        node
    | Proj i ->
        let kind = DProj i in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Eq ->
        let cmp_deps = Graph.get_dependencies g n in
        let set_node =
            {
              id = next_id ();
              kind = Set Eq;
              ir_node = n;
              in_regs = get_in_reg_mask (Set Eq);
              out_reg = get_out_reg_mask (Set Eq);
            }
        in
        (match
           List.find_map cmp_deps ~f:(fun n ->
               Option.bind n ~f:(fun n ->
                   match n.typ with
                   | Integer (Value _) -> Some n
                   | _ -> None))
         with
        | None ->
            let cmp_node =
                {
                  id = next_id ();
                  kind = Cmp;
                  ir_node = n;
                  in_regs = get_in_reg_mask Cmp;
                  out_reg = get_out_reg_mask Cmp;
                }
            in

            Graph.add_dependencies machine_g set_node [ Some cmp_node ];
            Graph.add_dependencies machine_g cmp_node
              (List.map cmp_deps ~f:(Option.map ~f:(convert_node g machine_g)))
        | Some cn ->
            let value =
                match cn.typ with
                | Integer (Value v) -> v
                | _ -> assert false (* already checked in the List.find call above *)
            in
            let cmp_node =
                {
                  id = next_id ();
                  kind = CmpImm value;
                  ir_node = n;
                  in_regs = get_in_reg_mask (CmpImm value);
                  out_reg = get_out_reg_mask (CmpImm value);
                }
            in
            Graph.add_dependencies machine_g set_node [ Some cmp_node ];
            let deps =
                List.filter cmp_deps ~f:(fun n ->
                    match n with
                    | None -> true
                    | Some n -> not (Node.equal cn n))
            in
            Graph.add_dependencies machine_g cmp_node
              (List.map deps ~f:(Option.map ~f:(convert_node g machine_g))));
        set_node
    | Phi ->
        let kind = Ideal Phi in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | _ -> assert false

and of_ctrl_node (g : Node.t Graph.t) (machine_g : t Graph.t) (kind : Node.ctrl_kind) (n : Node.t) =
    match kind with
    | If ->
        let deps = Graph.get_dependencies g n in
        let op =
            List.filter_opt deps
            |> List.find_map_exn ~f:(fun n ->
                   match n.kind with
                   | Data Eq -> Some Eq
                   | _ -> None)
        in
        let kind = Jmp op in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [];
        let deps = deps |> List.map ~f:(Option.map ~f:(convert_node g machine_g)) in
        Graph.add_dependencies machine_g node deps;
        node
    | Stop ->
        let node = Graph.get_stop machine_g in
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Start ->
        let node = Graph.get_start machine_g in
        node
    | Proj i ->
        let kind = Ideal (CProj i) in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Loop ->
        let kind = Ideal Loop in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node
    | Region ->
        let kind = Ideal Region in
        let in_regs = get_in_reg_mask kind
        and out_reg = get_out_reg_mask kind in
        let node = { id = next_id (); kind; ir_node = n; in_regs; out_reg } in
        Graph.add_dependencies machine_g node [];
        let deps =
            Graph.get_dependencies g n |> List.map ~f:(Option.map ~f:(convert_node g machine_g))
        in
        Graph.add_dependencies machine_g node deps;
        node

and convert_node (g : Node.t Graph.t) (machine_g : t Graph.t) (n : Node.t) =
    match
      Graph.find machine_g ~f:(fun mn ->
          match n.kind with
          | Ctrl Start
          | Ctrl Stop ->
              false
          | _ -> Node.equal mn.ir_node n)
    with
    | Some mn -> mn
    | None -> (
        match n.kind with
        | Data k -> of_data_node g machine_g k n
        | Ctrl k -> of_ctrl_node g machine_g k n
        | _ -> assert false)

let find_dep machine_g n ~f =
    Graph.get_dependencies machine_g n
    |> List.findi ~f:(fun _ dep ->
           match dep with
           | None -> false
           | Some dep -> f dep)
    |> Option.map ~f:(fun (i, n) -> (i, Option.value_exn n))

let post_process (machine_g : t Graph.t) =
    (* when changing a node's dependency we need to add a temp node that depends on the new_dep to make sure it doesn't get removed for not having any dependants. E.g. A jmp removes it's depedendancy on a set and set's it to the cmp directly. But the set might get removed if it has no dependants which in turn might remove the cmp for not having dependants *)
    let temp_node =
        {
          id = next_id ();
          kind = Int;
          in_regs = no_regs;
          out_reg = no_regs;
          ir_node = { typ = TOP; kind = Data Constant; id = 0 };
        }
    in
    Graph.fold machine_g ~init:[] ~f:(fun acc n ->
        match n.kind with
        | Jmp _ -> (
            let set_dep =
                find_dep machine_g n ~f:(fun dep ->
                    match dep.kind with
                    | Set _ -> true
                    | _ -> false)
            in
            let cmp_dep =
                Option.bind set_dep ~f:(fun (_, set) ->
                    find_dep machine_g set ~f:(fun dep ->
                        match dep.kind with
                        | Cmp
                        | CmpImm _ ->
                            true
                        | _ -> false))
            in
            match cmp_dep with
            | None -> acc
            | Some (_, cmp_n) ->
                let set_idx, set_n = Option.value_exn set_dep in
                (n, set_n, set_idx, cmp_n) :: acc)
        | _ -> acc)
    |> List.iter ~f:(fun (node, old_dep, idx, new_dep) ->
           Graph.add_dependencies machine_g temp_node [ Some new_dep ];
           Graph.remove_dependency machine_g ~node ~dep:old_dep;
           Graph.set_dependency machine_g node (Some new_dep) idx);
    Graph.remove_node machine_g temp_node

module MachineGraphNode : Graph.GraphNode with type t = t = struct
  type nonrec t = t

  let show = show
  let pp = pp
  let equal = equal
  let semantic_equal n _ n' _ = equal n n'
  let hash = hash
  let compare = compare
  let sexp_of_t = sexp_of_t
  let is_persistent = Fun.const false
end

let convert_graph (g : Node.t Graph.t) =
    let start =
        {
          id = next_id ();
          kind = Ideal Start;
          ir_node = Graph.get_start g;
          in_regs = no_regs;
          out_reg = no_regs;
        }
    in
    let stop =
        {
          id = next_id ();
          kind = Ideal Stop;
          ir_node = Graph.get_stop g;
          in_regs = no_regs;
          out_reg = no_regs;
        }
    in

    let machine_g = Graph.create (module MachineGraphNode) start stop in
    convert_node g machine_g stop.ir_node |> ignore;
    post_process machine_g;
    machine_g
