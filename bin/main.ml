open Dawn
open Core
open Dawn.Ast

let lex (s : string) : Tokens.token list =
    let lexbuf = Lexing.from_string s in
    let rec loop tokens =
        match Lexer.read lexbuf with
        | EOF -> List.rev (Tokens.EOF :: tokens)
        | token -> loop (token :: tokens)
    in
    loop []

let _ = Add (Int 4, Int 5)

(* let parse s = *)
(*     let ast = NParser.parse_string s in *)
(*     ast *)
let filename = "examples/test.eos"
let file = In_channel.read_lines filename |> String.concat_lines
let () = Printf.printf "%s" file
let () = Printf.printf "--------\n"
let () = file |> lex |> [%derive.show: Tokens.token list] |> Printf.printf "%s\n"
let () = Printf.printf "--------\n"

let () =
    match Parser.parse filename with
    | Ok ast -> ast |> Ast.show_program |> Printf.printf "%s\n"
    | Error msg -> Printf.eprintf "%s\n" msg
