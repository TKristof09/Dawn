open Lexing
module I = Menhir_parser.MenhirInterpreter

let get_parse_error env =
    match I.top env with
    | None -> "Invalid syntax"
    | Some (Element (current, _semv, _startp, _endp)) -> (
        try Parser_messages.message (I.number current) with
        | Not_found -> "invalid syntax (no specific message for this eror)")

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
    | I.Rejected -> raise (Lexer.SyntaxError (None, "invalid syntax (parser rejected the input)"))

let parse filename =
    let open Core in
    let contents = In_channel.read_all filename |> String.rstrip in
    let lexbuf = Lexing.from_string contents in
    Lexing.set_filename lexbuf filename;
    let checkpoint = Menhir_parser.Incremental.prog lexbuf.lex_curr_p in
    try parse_aux lexbuf checkpoint with
    | Lexer.SyntaxError (pos, msg) -> (
        match pos with
        | Some (line, col) ->
            Error (Printf.sprintf "%s:%d:%d: Syntax error: %s" filename line col msg)
        | None -> Error (Printf.sprintf "%s: Syntax error: %s" filename msg))

let parse_str str =
    let lexbuf = Lexing.from_string str in
    Lexing.set_filename lexbuf "str";
    let checkpoint = Menhir_parser.Incremental.prog lexbuf.lex_curr_p in
    try parse_aux lexbuf checkpoint with
    | Lexer.SyntaxError (pos, msg) -> (
        match pos with
        | Some (line, col) -> Error (Printf.sprintf "%d:%d: Syntax error: %s" line col msg)
        | None -> Error (Printf.sprintf "Syntax error: %s" msg))
