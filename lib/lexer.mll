{
    open Tokens

    let keywords = 
        let open Core in
        Hashtbl.of_alist_exn
            (module String)
            [
                (    "true",     TRUE);
                (   "false",    FALSE);
                (    "null",  NULLPTR);
                (      "if",       IF);
                (    "else",     ELSE);
                (   "while",    WHILE);
                (* (     "for",      FOR); *)
                (* (  "return",   RETURN); *)
                (* (   "break",    BREAK); *)
                (* ("continue", CONTINUE); *)
                (     "let",      LET);
                (     "fun",      FUN);
                (     "@extern",      EXTERN);
            ]
    
    exception SyntaxError of (int * int) option * string

    let get_lexing_position lexbuf =
        let open Lexing in
        let p = lexeme_start_p lexbuf in
        let line_number = p.pos_lnum in
        let column = p.pos_cnum - p.pos_bol + 1 in
        (line_number, column)

    let raise_error lexbuf msg = 
        let line, column = get_lexing_position lexbuf in
        raise (SyntaxError (Some (line, column), msg))
}
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = '@'? ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']


rule read = parse
    | white { read lexbuf }
    | newline { Lexing.new_line lexbuf; read lexbuf }
    | "//" [^ '\n']*  { read lexbuf }
    | "/*" { multi_line_comment lexbuf }
    | eof { EOF }
    | id as s { 
            match Core.Hashtbl.find keywords s with 
            | None -> IDENTIFIER s
            | Some token -> token
        }
    | digit+ as num { INT (int_of_string num) }
    | '"' { read_string (Buffer.create 16) lexbuf }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MUL }
    | "/" { DIV }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "," { COMMA }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "=" { ASSIGN }
    | "==" { EQUAL }
    | "!" { NOT }
    | "!=" { NOT_EQUAL }
    | "<" { LT }
    | ">" { GT } 
    | "<=" { LEQ }
    | ">=" { GEQ } 
    | "<<" { LSH }
    | ">>" { RSH }
    | "&" { BAND } 
    | "&&" { LAND }
    | "|" { BOR }
    | "||" { LOR }
    | "->" { ARROW }
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  (* escaped characters *)
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  (* normal characters *)
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise_error lexbuf ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { raise_error lexbuf ("String is not terminated") }
and multi_line_comment =
    parse
    | "*/" { read lexbuf }
    | '\n' { Lexing.new_line lexbuf; multi_line_comment lexbuf }
    | eof  { raise_error lexbuf "Unterminated comment" }
    | _    { multi_line_comment lexbuf }
