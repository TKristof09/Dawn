open Core

let asm_of_op (n : Machine_node.t) =
    match n.kind with
    | Add
    | AddImm _ ->
        "add"
    | Sub
    | SubImm _ ->
        "sub"
    | Cmp
    | CmpImm _ ->
        "cmp"
    | Mul
    | MulImm _ ->
        "imul"
    | Div -> "idiv"
    | JmpAlways -> "jmp"
    | Jmp cond -> (
        match cond with
        | Eq -> "je"
        | Neq -> "jne")
    | Int _ -> "mov"
    | Mov -> "mov"
    | Set cond -> (
        match cond with
        | Eq -> "sete"
        | Neq -> "setne")
    | DProj _ -> failwith "TODO"
    | Ideal _ -> ""

let asm_of_reg (reg : Registers.reg) =
    match reg with
    | Flags -> ""
    | _ -> Registers.show_reg reg |> String.lowercase

let get_label (n : Machine_node.t) =
    match n.kind with
    | Ideal Loop -> Printf.sprintf "Loop_%d" n.id
    | Ideal (CProj _) -> Printf.sprintf "L_%d" n.id
    | _ -> assert false

let asm_of_node g reg_assoc (n : Machine_node.t) =
    match n.kind with
    | Int i ->
        let reg = Hashtbl.find_exn reg_assoc n in
        let op_str = asm_of_op n in
        Printf.sprintf "\t%s %s, %d" op_str (asm_of_reg reg) i
    | AddImm i
    | SubImm i
    | CmpImm i ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        let regs = deps |> List.filter_opt |> List.map ~f:(Hashtbl.find_exn reg_assoc) in
        let op_str = asm_of_op n in
        let reg_str = regs |> List.map ~f:asm_of_reg |> String.concat ~sep:", " in
        Printf.sprintf "\t%s %s, %d" op_str reg_str i
    | Div ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        let reg_divisor = List.nth_exn deps 1 |> Option.value_exn |> Hashtbl.find_exn reg_assoc in
        let reg_str = asm_of_reg reg_divisor in
        let op_str = asm_of_op n in
        (* have to sign extend RAX into RDX for signed division *)
        (* TODO: use xor rdx, rdx  for unsigned division once we have that distinction *)
        Printf.sprintf "\tcqo\n\t%s %s \t\t// rax = rax / %s" op_str reg_str reg_str
    | Ideal Loop -> Printf.sprintf "%s:" (get_label n)
    | Ideal (CProj 0) -> Printf.sprintf "%s:" (get_label n)
    | Ideal _ -> ""
    | Jmp _ ->
        let op_str = asm_of_op n in
        let target =
            Graph.get_dependants g n
            |> List.find_exn ~f:(fun n ->
                   match n.kind with
                   | Ideal (CProj 0) -> true
                   | _ -> false)
        in
        let label_str = get_label target in
        Printf.sprintf "\t%s %s\n" op_str label_str
    | JmpAlways ->
        let op_str = asm_of_op n in
        let target = Graph.get_dependency g n 0 |> Option.value_exn in
        let label_str = get_label target in
        Printf.sprintf "\t%s %s\n" op_str label_str
    | _ ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        (* if not two address node, add the node itself to the start of the list to get the output reg too *)
        let nodes =
            if
              (not (Machine_node.is_two_address n))
              &&
              (* TODO this is ugly just to get cmp node assembly to not be weird *)
              match n.out_reg g n 0 with
              | None -> false
              | Some m when Registers.Mask.equal m Registers.Mask.flags -> false
              | Some _ -> true
            then
              Some n :: deps
            else
              deps
        in
        let regs = nodes |> List.filter_opt |> List.map ~f:(Hashtbl.find_exn reg_assoc) in
        let op_str = asm_of_op n in
        let reg_str = regs |> List.map ~f:asm_of_reg |> String.concat ~sep:", " in
        Printf.sprintf "\t%s %s" op_str reg_str

let add_loop_jumps g (program : Machine_node.t list) =
    let rec insert_after l target node_to_insert =
        match l with
        | [] -> []
        | h :: t when Machine_node.equal h target -> h :: node_to_insert :: t
        | h :: t -> h :: insert_after t target node_to_insert
    in

    let end_of_bb g program bb =
        List.fold_until (List.tl_exn program) ~init:None
          ~f:(fun acc n ->
            let cfg = if Machine_node.equal n bb then Some n else Graph.get_dependency g n 0 in
            match cfg with
            | None -> Continue acc
            | Some cfg -> (
                match acc with
                | None -> if Machine_node.equal bb cfg then Continue (Some n) else Continue None
                | Some n' -> if Machine_node.equal bb cfg then Continue (Some n) else Stop (Some n')
                ))
          ~finish:(fun res ->
            match res with
            | Some n -> Some n
            | None -> failwithf "Couldn't find end of bb %s" (Machine_node.show bb) ())
        |> Option.value_exn
    in
    let rec aux (l : Machine_node.t list) =
        match l with
        | [] -> []
        | h :: t -> (
            match h.kind with
            | Ideal Loop ->
                let backedge = Loop_node.get_back_edge g h in
                let end_of_backedge_bb = end_of_bb g l backedge in
                let jmp_node : Machine_node.t =
                    {
                      id = Machine_node.next_id ();
                      kind = JmpAlways;
                      ir_node = h.ir_node;
                      in_regs = Machine_node.get_in_reg_mask JmpAlways;
                      out_reg = Machine_node.get_out_reg_mask JmpAlways;
                    }
                in
                Graph.add_dependencies g jmp_node [ Some h ];
                let t = insert_after t end_of_backedge_bb jmp_node in
                h :: aux t
            | _ -> h :: aux t)
    in
    aux program

let invert_loop_conditions g (program : Machine_node.t list) =
    List.map program ~f:(fun n ->
        match n.kind with
        | Jmp cond -> (
            let cfg_parent = Graph.get_dependency g n 0 |> Option.value_exn in
            match cfg_parent.kind with
            | Ideal Loop ->
                n.kind <- Jmp (Machine_node.invert_cond cond);
                let children = Graph.get_dependants g n in
                List.iter children ~f:(fun c ->
                    match c.kind with
                    | Ideal (CProj i) -> c.kind <- Ideal (CProj (1 - i))
                    | _ -> ());
                n
            | _ -> n)
        | _ -> n)

let emit_program g reg_assoc program =
    add_loop_jumps g program
    |> invert_loop_conditions g
    |> List.map ~f:(fun n -> asm_of_node g reg_assoc n)
    |> List.filter ~f:(Fun.negate String.is_empty)
    |> String.concat ~sep:"\n"
