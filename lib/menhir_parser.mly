%{
    open Ast

    let make_node (start_pos, _) node =
        let open Lexing in
        {
            node = node;
            loc = {
                filename = start_pos.pos_fname;
                line = start_pos.pos_lnum;
                col = start_pos.pos_cnum - start_pos.pos_bol + 1;
            };
        }
%}
%token <string> INT
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
%token DOT

%token IF
%token ELSE
%token WHILE
(* %token FOR *)
(* %token RETURN *)
(* %token BREAK *)
(* %token CONTINUE *)

%token ASSIGN
%token LET
%token CONST
%token FUN
%token TYPE
%token ARROW
%token EXTERN
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
%left DOT
%nonassoc LPAREN

%start <program> prog
%%


let prog := terminated(statement*, EOF)


let statement :=
    | expr_statement
    | while_loop
    | LET; id = IDENTIFIER; t = option(preceded(COLON, type_name)); ASSIGN; e = terminated(expr, SEMICOLON); 
        {  
            match t with
            | None -> 
                (* TODO *)
                Declaration_assign (id, Type "", e, Mutable) |> make_node $sloc
            | Some t ->
                Declaration_assign (id, t, e, Mutable) |> make_node $sloc
        }
    | CONST; id = IDENTIFIER; t = option(preceded(COLON, type_name)); ASSIGN; e = terminated(expr, SEMICOLON); 
        {  
            match t with
            | None -> 
                (* TODO *)
                Declaration_assign (id, Type "", e, Const) |> make_node $sloc
            | Some t ->
                Declaration_assign (id, t, e, Const) |> make_node $sloc
        }
    | LET; id = IDENTIFIER; t = option(preceded(COLON, type_name)); SEMICOLON; 
        {  
            match t with
            | None ->
                Declaration (id, Type "") |> make_node $sloc 
            | Some t -> 
                Declaration (id, t) |> make_node $sloc 
        }


let param := id = IDENTIFIER; COLON; typ = type_name; { (id, typ) }

let expr_statement := 
    | e = terminated(expr, SEMICOLON); { ExprStatement e |> make_node $sloc }
    | e = block_statement; { ExprStatement e |> make_node $sloc }
    | e = ifelse_statement; { ExprStatement e |> make_node $sloc }

let expr := 
    | expr_without_block
    | expr_with_block
    | FUN; params = delimited(LPAREN, separated_list(COMMA, param), RPAREN); ret_t = option(preceded(ARROW, type_name)); body = block; 
        { 
            let param_types = List.map (snd) params in
            let param_names = List.map (fst) params in
            let ret_typ = 
                match ret_t with
                    | None -> Type "void"
                    | Some t -> t
            in
            let typ = Fn (ret_typ, param_types) in
            FnDeclaration (typ, param_names, body) |> make_node $sloc 
        }
    | FUN; params = delimited(LPAREN, separated_list(COMMA, param), RPAREN); ret_t = option(preceded(ARROW, type_name)); ASSIGN; EXTERN; LPAREN; external_name = STRING; RPAREN; 
        { 
            let param_types = List.map (snd) params in
            let param_names = List.map (fst) params in
            let ret_typ = 
                match ret_t with
                    | None -> Type "void"
                    | Some t -> t
            in
            let typ = Fn (ret_typ, param_types) in
            ExternalFnDeclaration (typ, param_names, external_name) |> make_node $sloc 
        }
    | TYPE; t = type_name; 
        { 
            TypeDeclaration t |> make_node $sloc
        }
    | id = IDENTIFIER; LBRACE; field_values = field_initialiser_list; RBRACE; 
        {
            TypeInstantiation(id, field_values) |> make_node $sloc
        }

let field_initialiser_list := 
    |  { [] }
    | field_values = separated_nonempty_list(COMMA, separated_pair(preceded(DOT, IDENTIFIER), ASSIGN, expr)); { List.map (fun (s,e) -> (Some s, e)) field_values }
    | field_values = separated_nonempty_list(COMMA, expr); { List.map (fun e -> (None, e)) field_values }

let expr_without_block := 
    | id = IDENTIFIER; ASSIGN; rhs = expr; {  VarAssign (id, rhs) |> make_node $sloc }
    | id = IDENTIFIER; n = delimited(LBRACKET, expr, RBRACKET); ASSIGN; rhs = expr; {  ArrayVarAssign (id, n, rhs) |> make_node $sloc }
    | MINUS; e = expr; %prec UMINUS {  Sub (make_node $sloc (Int (Z.zero)), e) |> make_node $sloc }
    | NOT; e = expr; %prec NOT { UNot e |> make_node $sloc } | bin_expr
    | e = expr; LPAREN; args = arg_list; RPAREN; {  FnCall(e, args) |> make_node $sloc }
    | literal
    | delimited(LPAREN, expr, RPAREN)
    | id = IDENTIFIER; n = option(delimited(LBRACKET, expr, RBRACKET)); { Variable (id, n) |> make_node $sloc }
    | base = expr; DOT; field = IDENTIFIER; { FieldAccess (base, field) |> make_node $sloc }

let bin_expr := 
    | lhs = expr; op = binop; rhs = expr; {  op (lhs,rhs) }
    | lhs = expr; LAND; rhs = expr; {  IfElse (lhs, rhs, Some (make_node $sloc (Bool false))) |> make_node $sloc }
    | lhs = expr; LOR; rhs = expr; {  IfElse (lhs, (make_node $sloc (Bool true)), Some rhs) |> make_node $sloc }


let arg_list := separated_list(COMMA, expr)

let expr_with_block := 
    | ifelse_expr
    | block_expr

let block := 
    | block_statement 
    | block_expr

let block_statement :=
    | l = delimited(LBRACE, statement*, RBRACE); {  Block(l, None) |> make_node $sloc }

let block_expr :=
    | delimited(LBRACE, block_expr_inside, RBRACE)

let block_expr_inside :=
    | e = expr; {  Block ([], Some e) |> make_node $sloc }
    | s = statement; rest = block_expr_inside_rest; {
      let statements, expr = rest in
      Block (s :: statements, expr) |> make_node $sloc
    }

let block_expr_inside_rest :=
    | e = expr; {  [], Some e }
    | s = statement; rest = block_expr_inside_rest; {
      let statements, expr = rest in
      (s :: statements, expr)
    }


let ifelse_expr := 
    IF; cond = delimited(LPAREN, expr, RPAREN); 
        body = block_expr; 
        else_body = option(preceded(ELSE, block_expr)); 
    {  IfElse (cond, body, else_body) |> make_node $sloc }

let ifelse_statement := 
    IF; cond = delimited(LPAREN, expr, RPAREN);
        body = block_statement;
        else_body = option(preceded(ELSE, block_statement));
    {  IfElse (cond, body, else_body) |> make_node $sloc }

let while_loop :=
    WHILE; cond = delimited(LPAREN, expr, RPAREN); body = block_statement; 
    {  While (cond, body) |> make_node $sloc }

let literal := 
    | x = STRING; { make_node $sloc (String x) }
    | x = INT; { make_node $sloc (Int (Z.of_string x)) }
    | FALSE; {  Bool (false)  |> make_node $sloc }
    | TRUE; {  Bool (true) |> make_node $sloc }
    | NULLPTR; {  Nullptr |> make_node $sloc }

let binop == 
    | PLUS; {  fun (e,e') -> Add (e,e') |> make_node $sloc }
    | MINUS; {  fun (e,e') -> Sub (e,e') |> make_node $sloc }
    | MUL; {  fun (e,e') -> Mul (e,e') |> make_node $sloc }
    | DIV; {  fun (e,e') -> Div (e,e') |> make_node $sloc }
    | LSH; {  fun (e,e') -> Lsh (e,e') |> make_node $sloc }
    | RSH; {  fun (e,e') -> Rsh (e,e') |> make_node $sloc }
    | BAND; {  fun (e,e') -> BAnd (e,e') |> make_node $sloc }
    | BOR; {  fun (e,e') -> BOr (e,e') |> make_node $sloc }
    | GT; {  fun (e,e') -> Gt (e,e') |> make_node $sloc }
    | GEQ; {  fun (e,e') -> GEq (e,e') |> make_node $sloc }
    | LT; {  fun (e,e') -> Lt (e,e') |> make_node $sloc }
    | LEQ; {  fun (e,e') -> LEq (e,e') |> make_node $sloc }
    | EQUAL; {  fun (e,e') -> Eq (e,e') |> make_node $sloc }
    | NOT_EQUAL; {  fun (e,e') -> NEq (e,e') |> make_node $sloc }

(* 1. Parses anything inside parentheses into a list of types *)
let paren_types :=
  | LPAREN; RPAREN; {[] }
  | LPAREN; t = type_name; RPAREN; { [t] }
  | LPAREN; t = type_name; COMMA; ts = separated_list(COMMA, type_name); RPAREN; { t :: ts }

(* 2. Atomic types that do NOT start with an open parenthesis (Identifiers, Arrays) *)
let non_paren_atomic_type :=
  | name = IDENTIFIER; { Type name }
  | t = atomic_type; n = delimited(LBRACKET, expr, RBRACKET); { Array (t, n) }

(* 3. General atomic type: either a non-paren type or a paren type *)
let atomic_type :=
  | t = non_paren_atomic_type; { t }
  | ts = paren_types; 
    { 
      match ts with
      | [single] -> single
      |[] -> failwith "Unit type () not supported yet"
      | _ -> failwith "Tuple types not supported yet"
    }

let type_name :=
  | t = fun_type_expression; { t }
  | t = atomic_type; { t }
  | t = struct_type_expression; { t }

let fun_type_expression := 
  | params = paren_types; ARROW; ret = atomic_type; { Fn(ret, params) }
  | param = non_paren_atomic_type; ARROW; ret = atomic_type; { Fn(ret,[param]) }

let struct_type_expression := 
  | LBRACE; l = separated_list(COMMA, separated_pair(IDENTIFIER, COLON, type_name)); RBRACE; 
    { 
        Struct l
    }
