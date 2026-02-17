open Dawn
open Core

let parse filename =
    match Parser.parse filename with
    | Ok ast -> Ast.show_program ast |> print_endline
    | Error msg -> Printf.eprintf "%s\n" msg

let opt filename =
    match Parser.parse filename with
    | Ok ast ->
        let linker = Linker.create () in
        let son = Son.of_ast ast linker in
        [%log.debug "\n%a" Ir_printer.pp_dot son];
        Sccp.run son;
        Graph.iter son ~f:(fun n ->
            match n.typ with
            | ALL -> [%log.error "Type error in %s" (Node.show n)]
            | _ -> ());
        [%log.debug "\n%s" (Ir_printer.to_dot son)]
    | Error msg -> Printf.eprintf "%s\n" msg

let compile filename =
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

let usage_msg =
    "USAGE: dawn [COMMAND] [OPTIONS] FILE\n\n\
     Commands:\n\
    \  compile    Compile the file (default)\n\
    \  parse      Parse and print AST\n\
    \  opt        Run sccp and print result\n\n\
     Options:"

let command = ref "compile"
let filename = ref ""

let rec speclist =
    [
      ( "-help",
        Stdlib.Arg.Unit
          (fun () ->
            Stdlib.Arg.usage speclist usage_msg;
            exit 0),
        " Display this help message" );
    ]

let anon_fun arg =
    if String.(!command = "compile") && String.(!filename = "") then
      (* First positional arg could be command or filename *)
      if String.(arg = "compile") || String.(arg = "parse") || String.(arg = "opt") then
        command := arg
      else
        filename := arg
    else if String.(!filename = "") then
      filename := arg
    else
      raise (Stdlib.Arg.Bad ("Unexpected argument: " ^ arg))

let () =
    Stdlib.Arg.parse speclist anon_fun usage_msg;
    if String.(!filename = "") then filename := "examples/test.eos";
    match !command with
    | "compile" -> compile !filename
    | "parse" -> parse !filename
    | "opt" -> opt !filename
    | _ ->
        Printf.eprintf "Unknown command: %s\n" !command;
        Stdlib.Arg.usage speclist usage_msg;
        exit 1
