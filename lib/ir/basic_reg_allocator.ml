open Core

module ComparableNode = struct
  include Node
  include Comparator.Make (Node)
end

let get_live_ranges (g : Node.t Graph.t) (program : Machine_node.t list) =
    let ranges = Hashtbl.create (module Node) in
    List.iter program ~f:(fun n ->
        match n.ir_node.kind with
        | Data Phi ->
            let depedencies = Graph.get_dependencies g n.ir_node |> List.filter_opt in
            let merged =
                Set.union_list
                  (module ComparableNode)
                  (List.filter_map depedencies ~f:(Hashtbl.find ranges))
            in
            let merged = Set.add merged n.ir_node in
            Set.iter merged ~f:(fun n -> Hashtbl.set ranges ~key:n ~data:merged)
        | _ ->
            Hashtbl.add ranges ~key:n.ir_node
              ~data:(Set.singleton (module ComparableNode) n.ir_node)
            |> ignore);
    ranges

let allocate (g : Node.t Graph.t) (program : Machine_node.t list) =
    let live_ranges = get_live_ranges g program in
    ignore live_ranges;
    program
