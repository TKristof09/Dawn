open Lexing
module I = Menhir_parser.MenhirInterpreter

let get_parse_error env =
    match I.stack env with
    | (lazy Nil) -> "Invalid syntax"
    | (lazy (Cons (I.Element (state, _, _, _), _))) -> (
        try Parser_messages.message (I.number state) with
        | Not_found -> "invalid syntax (no specific message for this eror)")

let parse filename =
    let rec parse_aux lexbuf checkpoint =
        match checkpoint with
        | I.InputNeeded _env ->
            let token = Lexer.read lexbuf in
            let startp = lexbuf.lex_start_p
            and endp = lexbuf.lex_curr_p in
            let checkpoint = I.offer checkpoint (token, startp, endp) in
            parse_aux lexbuf checkpoint
        | I.Shifting _
        | I.AboutToReduce _ ->
            let checkpoint = I.resume checkpoint in
            parse_aux lexbuf checkpoint
        | I.HandlingError _env ->
            let line, pos = Lexer.get_lexing_position lexbuf in
            let err = get_parse_error _env in
            raise (Lexer.SyntaxError (Some (line, pos), err))
        | I.Accepted v -> Ok v
        | I.Rejected ->
            raise (Lexer.SyntaxError (None, "invalid syntax (parser rejected the input)"))
    in
    let open Core in
    let contents = In_channel.read_all filename |> String.rstrip in
    let lexbuf = Lexing.from_string contents in
    (* let lexbuf = Lexing.from_string ~with_positions:true "print(board[5])" in *)
    let checkpoint = Menhir_parser.Incremental.prog lexbuf.lex_curr_p in
    try parse_aux lexbuf checkpoint with
    | Lexer.SyntaxError (pos, msg) -> (
        match pos with
        | Some (line, col) ->
            Error (Printf.sprintf "%s:%d:%d: Syntax error: %s" filename line col msg)
        | None -> Error (Printf.sprintf "%s: Syntax error: %s" filename msg))
