open Core

let asm_of_op (kind : Machine_node.machine_node_kind) =
    match kind with
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
    | Lsh
    | LshImm _ ->
        "sal"
    | Rsh
    | RshImm _ ->
        "sar"
    | And
    | AndImm _ ->
        "and"
    | Or
    | OrImm _ ->
        "or"
    | JmpAlways -> "jmp"
    | Jmp cond -> (
        (* TODO: signed vs unsigned have different names *)
        match cond with
        | Eq -> "je"
        | NEq -> "jne"
        | Lt -> "jl"
        | LEq -> "jle"
        | Gt -> "jg"
        | GEq -> "jge")
    | Int _ -> "mov"
    | Mov -> "mov"
    | Set cond -> (
        (* TODO: signed vs unsigned have different names *)
        match cond with
        | Eq -> "sete"
        | NEq -> "setne"
        | Lt -> "setl"
        | LEq -> "setle"
        | Gt -> "setg"
        | GEq -> "setge")
    | DProj _ -> failwith "TODO"
    | Ideal _ -> ""

let asm_of_loc (loc : Registers.loc) =
    match loc with
    | Reg Flags -> ""
    | Reg reg -> Registers.show_reg reg |> String.lowercase
    | Stack offs -> Printf.sprintf "[rsp %s %d]" (if offs > 0 then "+" else "-") (abs offs)

let get_label (n : Machine_node.t) =
    match n.kind with
    | Ideal Loop -> Printf.sprintf "Loop_%d" n.id
    | Ideal Stop -> "Exit"
    | Ideal _ -> Printf.sprintf "L_%d" n.id
    | _ -> failwithf "get_label invalid node: %s" (Machine_node.show n) ()

let is_jmp_target g (n : Machine_node.t) =
    Poly.equal n.kind (Ideal Region)
    || List.exists (Graph.get_dependencies g n) ~f:(fun (n' : Machine_node.t option) ->
           match n' with
           | Some { kind = Jmp _; _ }
           | Some { kind = JmpAlways; _ } ->
               true
           | Some ({ kind = Ideal (CProj 1); _ } as n') ->
               (* TODO: these useless CProj nodes should just get removed, that would be way cleaner *)
               List.length (Graph.get_dependants g n') = 1
           | _ -> false)

let get_first_blockhead g n =
    let n' = ref (Graph.get_dependants g n |> List.find_exn ~f:Machine_node.is_control_node) in
    while not (Machine_node.is_blockhead !n') do
      n' := Graph.get_dependants g !n' |> List.find_exn ~f:Machine_node.is_control_node
    done;
    !n'

let asm_of_node g reg_assoc (n : Machine_node.t) =
    match n.kind with
    | Int i ->
        let reg = Hashtbl.find_exn reg_assoc n in
        let op_str = asm_of_op n.kind in
        Printf.sprintf "\t%s %s, %d" op_str (asm_of_loc reg) i
    | AddImm i
    | SubImm i
    | MulImm i
    | CmpImm i
    | LshImm i
    | RshImm i
    | AndImm i
    | OrImm i ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        let regs = deps |> List.filter_opt |> List.map ~f:(Hashtbl.find_exn reg_assoc) in
        let op_str = asm_of_op n.kind in
        let reg_str = regs |> List.map ~f:asm_of_loc |> String.concat ~sep:", " in
        Printf.sprintf "\t%s %s, %d" op_str reg_str i
    | Div ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        let reg_divisor = List.nth_exn deps 1 |> Option.value_exn |> Hashtbl.find_exn reg_assoc in
        let reg_str = asm_of_loc reg_divisor in
        let op_str = asm_of_op n.kind in
        (* have to sign extend RAX into RDX for signed division *)
        (* TODO: use xor rdx, rdx  for unsigned division once we have that distinction *)
        Printf.sprintf "\tcqo\n\t%s %s \t\t// rax = rax / %s" op_str reg_str reg_str
    | Ideal _ ->
        if is_jmp_target g n then
          Printf.sprintf "%s:" (get_label n)
        else
          ""
    | Jmp _ ->
        let _true_branch =
            Graph.get_dependants g n
            |> List.find_exn ~f:(fun n ->
                   match n.kind with
                   | Ideal (CProj 0) -> true
                   | _ -> false)
        in
        let false_branch =
            Graph.get_dependants g n
            |> List.find_exn ~f:(fun n ->
                   match n.kind with
                   | Ideal (CProj 1) -> true
                   | _ -> false)
        in
        let op_str, label_str =
            if List.length (Graph.get_dependants g false_branch) = 1 then
              let next_false_bb = get_first_blockhead g false_branch in
              (asm_of_op (Machine_node.invert_jmp n.kind), get_label next_false_bb)
            else
              (asm_of_op (Machine_node.invert_jmp n.kind), get_label false_branch)
        in
        Printf.sprintf "\t%s %s\n" op_str label_str
    | JmpAlways ->
        let op_str = asm_of_op n.kind in
        let target = Graph.get_dependants g n |> List.hd_exn in
        let label_str = get_label target in
        Printf.sprintf "\t%s %s\n" op_str label_str
    | Lsh
    | Rsh ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        let reg = deps |> List.hd_exn |> Option.value_exn |> Hashtbl.find_exn reg_assoc in
        let op_str = asm_of_op n.kind in
        Printf.sprintf "\t%s %s, cl" op_str (asm_of_loc reg)
    | Add
    | Sub
    | Mul
    | And
    | Or
    | Cmp
    | Mov ->
        let deps = Graph.get_dependencies g n |> List.tl_exn in
        (* if not two address node, add the node itself to the start of the list to get the output reg too *)
        let nodes =
            if
              (not (Machine_node.is_two_address n))
              &&
              (* TODO this is ugly just to get cmp node assembly to not be weird *)
              match Machine_node.get_out_reg_mask g n 0 with
              | None -> false
              | Some m when Registers.Mask.equal m Registers.Mask.flags -> false
              | Some _ -> true
            then
              Some n :: deps
            else
              deps
        in
        let regs = nodes |> List.filter_opt |> List.map ~f:(Hashtbl.find_exn reg_assoc) in
        let op_str = asm_of_op n.kind in
        let reg_str = regs |> List.map ~f:asm_of_loc |> String.concat ~sep:", " in
        Printf.sprintf "\t%s %s" op_str reg_str
    | Set _
    | DProj _ ->
        failwith "TODO"

let add_jumps g (program : Machine_node.t list) =
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
    let rec aux (l : Machine_node.t list) res =
        match l with
        | [] -> List.rev res
        | h :: t -> (
            match h.kind with
            | Ideal Loop ->
                let backedge = Loop_node.get_back_edge g h in
                let end_of_backedge_bb = end_of_bb g l backedge in
                let jmp_node : Machine_node.t =
                    { id = Machine_node.next_id (); kind = JmpAlways; ir_node = h.ir_node }
                in
                Graph.add_dependencies g jmp_node [ Some backedge ];
                Graph.set_dependency g h (Some jmp_node) 1;
                (* Graph.add_dependencies g jmp_node [ Some h ]; *)
                let t = insert_after t end_of_backedge_bb jmp_node in
                aux t (h :: res)
            | Ideal Region ->
                let true_branch = Graph.get_dependency g h 1 |> Option.value_exn in
                let false_branch = Graph.get_dependency g h 2 |> Option.value_exn in
                (* if no else branch just fall through *)
                if List.length (Graph.get_dependants g false_branch) = 1 then
                  aux t (h :: res)
                else
                  let end_of_true_branch_bb = end_of_bb g program true_branch in
                  let jmp_node : Machine_node.t =
                      { id = Machine_node.next_id (); kind = JmpAlways; ir_node = h.ir_node }
                  in
                  Graph.add_dependencies g jmp_node [ Some true_branch ];
                  Graph.set_dependency g h (Some jmp_node) 1;
                  (* Graph.add_dependencies g jmp_node [ Some h ]; *)
                  (* Ewww *)
                  let res =
                      insert_after (List.rev res) end_of_true_branch_bb jmp_node |> List.rev
                  in
                  aux t (h :: res)
            | _ -> aux t (h :: res))
    in
    aux program []

(* let invert_loop_conditions g (program : Machine_node.t list) = *)
(*     List.map program ~f:(fun n -> *)
(*         match n.kind with *)
(*         | Jmp _ -> ( *)
(*             let cfg_parent = Graph.get_dependency g n 0 |> Option.value_exn in *)
(*             match cfg_parent.kind with *)
(*             | Ideal Loop -> *)
(*                 n.kind <- Machine_node.invert_jmp n.kind; *)
(*                 let children = Graph.get_dependants g n in *)
(*                 List.iter children ~f:(fun c -> *)
(*                     match c.kind with *)
(*                     | Ideal (CProj i) -> c.kind <- Ideal (CProj (1 - i)) *)
(*                     | _ -> ()); *)
(*                 n *)
(*             | _ -> n) *)
(*         | _ -> n) *)

let emit_program g reg_assoc program =
    add_jumps g program
    (* |> invert_loop_conditions g *)
    |> List.map ~f:(fun n -> asm_of_node g reg_assoc n)
    |> List.filter ~f:(Fun.negate String.is_empty)
    |> String.concat ~sep:"\n"
