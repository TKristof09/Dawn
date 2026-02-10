open Dawn
open Core

(* let lex (s : string) : Tokens.token list = *)
(*     let lexbuf = Lexing.from_string s in *)
(*     let rec loop tokens = *)
(*         match Lexer.read lexbuf with *)
(*         | EOF -> List.rev (Tokens.EOF :: tokens) *)
(*         | token -> loop (token :: tokens) *)
(*     in *)
(*     loop [] *)

(* let parse s = *)
(*     let ast = NParser.parse_string s in *)
(*     ast *)
let filename =
    if Array.length (Sys.get_argv ()) < 2 then
      "examples/test.eos" (* default filename *)
    else
      (Sys.get_argv ()).(1)

(* let file = In_channel.read_lines filename |> String.concat_lines *)
(* let () = Printf.printf "%s" file *)
(* let () = Printf.printf "--------\n" *)
(* let () = file |> lex |> [%derive.show: Tokens.token list] |> Printf.printf "%s\n" *)
(* let () = Printf.printf "--------\n" *)

let () =
    match Parser.parse filename with
    | Ok ast ->
        let linker = Linker.create () in
        let son = Son.of_ast ast linker in
        let son = Graph.readonly son in
        Ir_printer.to_dot son |> Printf.printf "\n\n%s\n";
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
        let code = Asm_emit.emit_program functions linker in
        let outfile = Filename.chop_extension filename ^ ".asm" in
        Printf.printf "Output: %s\n" outfile;
        Out_channel.write_all outfile ~data:code
    | Error msg -> Printf.eprintf "%s\n" msg
