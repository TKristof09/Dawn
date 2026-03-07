open Core

type t = {
    functions : (string * Node.t) Dynarray.t;
    mutable universe : Int.Set.t; (* cache it to not have to recreate the set on each link call *)
  }

let create () =
    (* function idx 0 is for the toplevel but this can never be called/linked
       to so we don't put it in the universe set *)
    { functions = Dynarray.create (); universe = Int.Set.empty }

let define linker fun_node =
    let idx = Dynarray.length linker.functions in
    let name = Printf.sprintf "Anon_fn_%d" idx in
    Dynarray.add_last linker.functions (name, fun_node);
    linker.universe <- Set.add linker.universe (idx + 1);
    idx + 1
(* we keep idx 0 for start node i.e. top level nodes that aren't in a function. TODO: remove if we decide to use a "main" functon *)

let get_name linker idx = Dynarray.get linker.functions (idx - 1) |> fst

let set_name linker idx new_name =
    let fun_node = Dynarray.get linker.functions (idx - 1) |> snd in
    Dynarray.set linker.functions (idx - 1) (new_name, fun_node)

let link linker g call_node =
    assert (Core.Poly.equal call_node.Node.kind (Ctrl FunctionCall));
    let fun_nodes = ref [] in
    let fun_ptr = Fun_node.get_call_fun_ptr g call_node in
    Types.iter_fun_indices fun_ptr.typ linker.universe ~f:(fun fun_idx ->
        let fun_node = Dynarray.get linker.functions (fun_idx - 1) |> snd in
        [%log.debug "Linking %a to %s" Node.pp call_node (get_name linker fun_idx)];
        if List.mem (Graph.get_dependants g call_node) fun_node ~equal:Node.equal then
          ()
        else (
          Fun_node.link_call g ~call_node ~fun_node;
          fun_nodes := fun_node :: !fun_nodes));
    !fun_nodes
