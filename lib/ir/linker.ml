type t = Node.t Dynarray.t

let create () = Dynarray.create ()

let define linker fun_node =
    let idx = Dynarray.length linker in
    Dynarray.add_last linker fun_node;
    idx + 1
(* we keep idx 0 for start node i.e. top level nodes that aren't in a function. TODO: remove if we decide to use a "main" functon *)

let get_fun_node linker (fun_ptr : Node.t) =
    let fun_idx = Types.get_fun_idx fun_ptr.typ in
    Dynarray.get linker (fun_idx - 1)

let link linker g call_node =
    let fun_ptr = Fun_node.get_call_fun_ptr g call_node in
    let fun_node = get_fun_node linker fun_ptr in
    Fun_node.link_call g ~call_node ~fun_node
