open Dawn

(* let test_str = *)
(*     {| *)
(*     let i:int = 0; *)
(*     let sum:int = 0; *)
(*     let c: int = 5; *)
(*     while(i == c - 1) { *)
(*         i = i + 1; *)
(*         let j:int = 0; *)
(*         while(i+j == 11){ *)
(*             sum = j; *)
(*             j = j + 2; *)
(*         } *)
(*     } *)
(*     if(sum == 0) {} *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let i:int = 0; *)
(*     let c: int = 5; *)
(*     let sum: int = 1; *)
(*     while(i == c - 1) { *)
(*         let j:int = 0; *)
(*         i = i + 1; *)
(*         j = j + 2; *)
(*         sum = sum + j; *)
(*     } *)
(*     if(sum == 1) { *)
(*     } *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let i:int = 0; *)
(*     while(i == 1) { *)
(*         i = i + 1; *)
(*         let j:int = 0; *)
(*         while(j == 10){ *)
(*             j = j + 2; *)
(*         } *)
(*     } *)
(*     |} *)
(* let fibo_str = *)
(*     {| *)
(*     let x:int = 0; *)
(*     let y:int = 1; *)
(*     while(0 <= x) { *)
(*         let tmp:int = x + y; *)
(*         x = y; *)
(*         y = tmp; *)
(*     } *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let k:int = 69; *)
(*     let i:int = k; *)
(*     let j:int = i / 69; *)
(*     if(i == j) {j = j+1;} *)
(*     if(j==0){j=j+4;} *)
(*     |} *)

(* let test_str = *)
(*     {| *)
(*     let k:int = 69; *)
(*     let i:int = k; *)
(*     let j:int = i / 69; *)
(*     let x:int = i / 420; *)
(*     i = i + 1; *)
(*     if(i == j) {j = j+1;} *)
(*     if(j==0){j=j+4;} *)
(*     if(x==j){x=j+1;} *)
(*     |} *)

(* let test_str = {| *)
(*     let i:int = 0; *)
(*     i = i | (1 << 2); *)
(*     if((i>>1) == 1) {} *)
(*     |} *)

(* let test_str = *)
(*     {| *)
(*     fun f(a: int, b: int, c: int, d: int, e: int, f: int) -> int { *)
(*         69 - a + b + c + d + e + f *)
(*     } *)
(**)
(*     let i: int = f(1,2,3,4,5,6) + 69; *)
(*     if(i==0) {} *)
(*     |} *)

(* let test_str = *)
(*     {| *)
(*     let k:int = 69; *)
(*     let i:int = k; *)
(*     let j:int = i / 69; *)
(*     let x:int = i / 420; *)
(*     i = i + 1; *)
(*     if(i == j) {j = j+1;} *)
(*     if(j==0){j=j+4;} *)
(*     if(x==j){x=j+1;} *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let x:int = 0; *)
(*     let y:int = 1; *)
(*     while(0 <= x) { *)
(*         let tmp:int = x + y; *)
(*         x = y; *)
(*         y = tmp; *)
(*     } *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let arr: int[20]; *)
(*     arr[0] = 1; *)
(*     arr[1] = 1; *)
(*     arr[2] = arr[0] + arr[1]; *)
(*     if(arr[1] == arr[2]){} *)
(*     |} *)
let test_str =
    {|
    fun print_int(i: int) -> int = @extern("print_int");
    if(print_int(2) == 1) {}
    |}

let () =
    match Parser.parse_str test_str with
    | Ok ast ->
        let linker = Linker.create () in
        let son = Son.of_ast ast linker in
        let son = Graph.readonly son in
        (* Ir_printer.to_dot son |> Printf.printf "\n\n%s\n"; *)
        let schedules = Scheduler.schedule son in

        (* let program, reg_assoc = Basic_reg_allocator.allocate machine_graph program in *)
        (* Ir_printer.to_dot_machine machine_graph |> Printf.printf "\n\n%s\n"; *)

        (* only do code gen on non external functions *)
        let functions =
            Core.List.filter schedules ~f:(fun (g, _) ->
                Graph.find g ~f:(fun n ->
                    match n.kind with
                    | Ideal (External _) -> true
                    | _ -> false)
                |> Option.is_none)
            |> Core.List.map ~f:(fun (g, program) ->
                Ir_printer.to_dot_machine g |> Printf.printf "\n\n%s\n";
                let flat_program = List.concat program in
                (* Ir_printer.to_string_machine_linear g flat_program |> print_endline; *)
                let program, reg_assignment = Reg_allocator.allocate g flat_program in
                Ir_printer.to_string_machine_linear_regs g program reg_assignment
                |> Printf.printf "\n\n%s\n";
                (g, reg_assignment, program))
        in
        Asm_emit.emit_program functions linker |> print_endline
    | Error msg -> Printf.eprintf "%s\n" msg
