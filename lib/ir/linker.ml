open Core

type t = (string * Node.t) Dynarray.t

let create () = Dynarray.create ()

let define linker fun_node =
    let idx = Dynarray.length linker in
    let name = Printf.sprintf "Anon_fn_%d" idx in
    Dynarray.add_last linker (name, fun_node);
    idx + 1
(* we keep idx 0 for start node i.e. top level nodes that aren't in a function. TODO: remove if we decide to use a "main" functon *)

let get_name linker idx = Dynarray.get linker (idx - 1) |> fst

let set_name linker idx new_name =
    let fun_node = Dynarray.get linker (idx - 1) |> snd in
    Dynarray.set linker (idx - 1) (new_name, fun_node)

let link linker g call_node =
    assert (Core.Poly.equal call_node.Node.kind (Ctrl FunctionCall));
    let fun_nodes = ref [] in
    let fun_ptr = Fun_node.get_call_fun_ptr g call_node in
    Types.iter_fun_indices fun_ptr.typ ~f:(fun fun_idx ->
        let fun_node = Dynarray.get linker (fun_idx - 1) |> snd in
        [%log.debug "Linking %a to %s" Node.pp call_node (get_name linker fun_idx)];
        if List.mem (Graph.get_dependants g call_node) fun_node ~equal:Node.equal then
          ()
        else (
          Fun_node.link_call g ~call_node ~fun_node;
          fun_nodes := fun_node :: !fun_nodes));
    !fun_nodes
