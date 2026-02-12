open Dawn
open Core

let filename =
    if Array.length (Sys.get_argv ()) < 2 then
      "examples/test.eos" (* default filename *)
    else
      (Sys.get_argv ()).(1)

let () =
    match Parser.parse filename with
    | Ok ast ->
        let linker = Linker.create () in
        let son = Son.of_ast ast linker in
        let son = Graph.readonly son in
        [%log.debug "\n%a" Ir_printer.pp_dot son];
        let schedules = Scheduler.schedule son in
        (* only do code gen on non external functions *)
        let functions =
            Core.List.filter schedules ~f:(fun (g, _) ->
                Graph.find g ~f:(fun n ->
                    match n.kind with
                    | Ideal (External _) -> true
                    | _ -> false)
                |> Option.is_none)
            |> Core.List.map ~f:(fun (g, program) ->
                [%log.debug "\n%a" Ir_printer.pp_dot_machine g];
                let flat_program = List.concat program in
                [%log.debug "\n%a" Ir_printer.pp_machine_linear (g, flat_program)];
                let program, reg_assignment = Reg_allocator.allocate g flat_program in
                [%log.debug "\n%a" Ir_printer.pp_machine_linear_regs (g, program, reg_assignment)];
                (g, reg_assignment, program))
        in
        let code = Asm_emit.emit_program functions linker in
        let outfile = Filename.chop_extension filename ^ ".asm" in
        [%log.info "Output: %s" outfile];

        Out_channel.write_all outfile ~data:code
    | Error msg -> Printf.eprintf "%s\n" msg
