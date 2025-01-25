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
        (* ast |> Ast.show_program |> Printf.printf "%s\n"; *)
        if Type_checker.check ast then (
          let code = Code_gen.gen ast in
          (* code *)
          (* |> [%derive.show: Asm.instruction list * Asm.instruction list] *)
          (* |> Printf.printf "%s\n"; *)
          let outfile = Filename.chop_extension filename ^ ".asm" in
          Printf.printf "Output: %s\n" outfile;
          Assembler.assemble code outfile)
    | Error msg -> Printf.eprintf "%s\n" msg
