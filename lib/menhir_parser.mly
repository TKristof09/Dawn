%{
    open Ast
%}
%token <int> INT
%token <string> STRING
%token <string> IDENTIFIER
%token TRUE
%token FALSE

%token NULLPTR
%token PLUS
%token MINUS
%token MUL
%token DIV

%token LSH
%token RSH
%token BAND
%token BOR

%token LAND
%token LOR
%token EQUAL
%token NOT_EQUAL
%token LT
%token GT
%token LEQ
%token GEQ
%token NOT

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token COLON
%token SEMICOLON

%token IF
%token ELSE
%token WHILE
(* %token FOR *)
(* %token RETURN *)
(* %token BREAK *)
(* %token CONTINUE *)

%token ASSIGN
%token LET
(* %token FUN *)
(* %token ARROW *)
%token EOF

%right ASSIGN
%left LOR
%left LAND
%left BOR
%left BAND
%left EQUAL NOT_EQUAL
%left GT GEQ LT LEQ
%left LSH RSH
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS NOT

%start <program> prog
%%

let prog := terminated(statement*, EOF)

let statement :=
    | expr_statement
    | while_loop
    | LET; id = IDENTIFIER; COLON; t = IDENTIFIER; ASSIGN; e = terminated(expr, SEMICOLON); { Declaration_assign (id, Type t, e) }
    | LET; id = IDENTIFIER; COLON; t = IDENTIFIER; n = delimited(LBRACKET, expr, RBRACKET); SEMICOLON; { Declaration (id, Array (t, n)) }
    | LET; id = IDENTIFIER; COLON; t = IDENTIFIER; SEMICOLON; { Declaration (id, Type t) }

let expr_statement := 
    | ~ = terminated(expr, SEMICOLON); <ExprStatement>
    | ~ = block_statement; <ExprStatement>
    | ~ = ifelse_statement; <ExprStatement>

let expr := 
    | expr_without_block
    | expr_with_block

let expr_without_block := 
    | id = IDENTIFIER; ASSIGN; rhs = expr; { VarAssign (id, rhs) }
    | id = IDENTIFIER; n = delimited(LBRACKET, expr, RBRACKET); ASSIGN; rhs = expr; { ArrayVarAssign (id, n, rhs) }
    | MINUS; ~ = expr; %prec UMINUS <UMinus>
    | NOT; ~ = expr; %prec NOT <UNot>
    | lhs = expr; op = binop; rhs = expr; { op (lhs,rhs) }
    | id = IDENTIFIER; LPAREN; args = arg_list; RPAREN; { FnCall(id, args) }
    | literal
    | delimited(LPAREN, expr, RPAREN)
    | ~ = IDENTIFIER; ~ = option(delimited(LBRACKET, expr, RBRACKET)); <Variable>

let arg_list := separated_list(COMMA, expr)

let expr_with_block := 
    | ifelse_expr
    | block_expr

let block_statement :=
    | l = delimited(LBRACE, statement*, RBRACE); { Block(l, None) }

let block_expr :=
    | LBRACE; ~ = block_expr_inside; RBRACE; <>

let block_expr_inside :=
    | e = expr; { Block ([], Some e) }
    | s = statement; rest = block_expr_inside_rest; {
      let statements, expr = rest in
      Block (s :: statements, expr)
    }

let block_expr_inside_rest :=
    | e = expr; { [], Some e }
    | s = statement; rest = block_expr_inside_rest; {
      let statements, expr = rest in
      (s :: statements, expr)
    }


let ifelse_expr := 
    IF; cond = delimited(LPAREN, expr, RPAREN); 
        body = block_expr; 
        else_body = option(preceded(ELSE, block_expr)); 
    { IfElse (cond, body, else_body) }

let ifelse_statement := 
    IF; cond = delimited(LPAREN, expr, RPAREN);
        body = block_statement;
        else_body = option(preceded(ELSE, block_statement));
    { IfElse (cond, body, else_body) }

let while_loop :=
    WHILE; cond = delimited(LPAREN, expr, RPAREN); body = block_statement; { While (cond, body) }

let literal := 
    | ~ = STRING; <String>
    | ~ = INT; <Int>
    | FALSE; { Bool (false) }
    | TRUE; { Bool (true) }
    | NULLPTR; { Nullptr }

let binop == 
    | PLUS; { fun (e,e') -> Add (e,e') }
    | MINUS; { fun (e,e') -> Sub (e,e') }
    | MUL; { fun (e,e') -> Mul (e,e') }
    | DIV; { fun (e,e') -> Div (e,e') }
    | LSH; { fun (e,e') -> Lsh (e,e') }
    | RSH; { fun (e,e') -> Rsh (e,e') }
    | BAND; { fun (e,e') -> BAnd (e,e') }
    | BOR; { fun (e,e') -> BOr (e,e') }
    | GT; { fun (e,e') -> Gt (e,e') }
    | GEQ; { fun (e,e') -> GEq (e,e') }
    | LT; { fun (e,e') -> Lt (e,e') }
    | LEQ; { fun (e,e') -> LEq (e,e') }
    | EQUAL; { fun (e,e') -> Eq (e,e') }
    | NOT_EQUAL; { fun (e,e') -> NEq (e,e') }
    | LAND; { fun (e,e') -> LAnd (e,e') }
    | LOR; { fun (e,e') -> LOr (e,e') }
