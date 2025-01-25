open Core
open Ast
open Asm
open Dlist
module H = Hashtbl.Make (String)

type symbol_info = {
    offset : int;
    size : int;
    element_size : int;
  }

type ctx = {
    parent : ctx option;
    symbols : symbol_info H.t;
    current_offset : int;
    current_label : int ref;
  }

let rec get_symbol_info sym ctx =
    match Hashtbl.find ctx.symbols sym with
    | Some info -> info
    | None -> (
        match ctx.parent with
        | Some parent -> get_symbol_info sym parent
        (* the program is already type checked so this cant happen *)
        | None -> assert false)

(* TODO *)
let get_size (typ : var_type) =
    match typ with
    | Type _ -> 8
    | Array (_, { loc = _; node = Int n }) -> n * 8
    | Array _ -> failwith "Only arrays with constant size are supported for now"
    | _ -> 8

let add_symbol ctx name typ =
    match typ with
    | Array (el, _) ->
        let size = get_size typ in
        let element_size = get_size el in
        let new_offset = ctx.current_offset - size in
        Hashtbl.set ctx.symbols ~key:name ~data:{ size; offset = new_offset; element_size };
        { ctx with current_offset = new_offset }
    | _ ->
        let size = get_size typ in
        let element_size = size in
        let new_offset = ctx.current_offset - size in
        Hashtbl.set ctx.symbols ~key:name ~data:{ size; offset = new_offset; element_size };
        { ctx with current_offset = new_offset }

let get_label ctx =
    let l = ctx.current_label in
    incr ctx.current_label;
    sprintf ".L%d" !l

let get_fn_arg_register i =
    match i with
    | 0 -> RDI
    | 1 -> RSI
    | 2 -> RDX
    | 3 -> RCX
    | 4 -> R8
    | 5 -> R9
    | _ -> failwith "Max 6 arguments go in registers"

let rec generate_statement node ctx =
    match node.node with
    | Declaration (name, typ) ->
        let size = get_size typ in
        let new_ctx = add_symbol ctx name typ in
        (new_ctx, singleton (Sub (Reg RSP, Imm size)))
    | Declaration_assign (name, typ, expr) ->
        let expr_instrs = generate_expression ctx expr in
        let new_ctx = add_symbol ctx name typ in
        let info = get_symbol_info name new_ctx in
        let prologue = singleton (Sub (Reg RSP, Imm info.size)) in
        (new_ctx, prologue @ expr_instrs @ singleton (Mov (Mem (RBP, info.offset), Reg RAX)))
    | FnDeclaration (name, Fn (_, param_types), param_names, body) ->
        (* let ctx = add_symbol ctx name (Fn (ret, param_types)) in *)
        let fn_ctx =
            List.fold2_exn param_names param_types
              ~init:
                {
                  parent = Some ctx;
                  symbols = H.create ();
                  current_offset = ctx.current_offset;
                  current_label = ref 0;
                }
              ~f:add_symbol
        in
        let total_param_size = ctx.current_offset - fn_ctx.current_offset in
        let prologue = of_list [ Label name; Enter total_param_size ] in
        let param_loads =
            List.foldi param_names ~init:(empty ()) ~f:(fun i acc name ->
                let info = get_symbol_info name fn_ctx in
                let reg = get_fn_arg_register i in
                append acc (singleton (Mov (Mem (RBP, info.offset), Reg reg))))
        in
        let epilogue = of_list [ Leave; Ret ] in
        (ctx, prologue @ param_loads @ generate_expression fn_ctx body @ epilogue)
    | FnDeclaration _ -> assert false
    | ExprStatement expr -> (ctx, generate_expression ctx expr)
    | While (cond, body) ->
        let cond_label = get_label ctx in
        let body_label = get_label ctx in
        let prologue = of_list [ Jmp cond_label; Label body_label ] in
        let epilogue = of_list [ Test (Reg RAX, Reg RAX); Jmp_cond (Nz, body_label) ] in
        let instructions =
            prologue
            @ generate_expression ctx body
            @ singleton (Label cond_label)
            @ generate_expression ctx cond
            @ epilogue
        in
        (ctx, instructions)

and generate_expression ctx node =
    let get_cond expr =
        match expr with
        | Ast.Gt _ -> Gt
        | Ast.GEq _ -> GEq
        | Ast.Lt _ -> Lt
        | Ast.LEq _ -> LEq
        | Ast.Eq _ -> Eq
        | Ast.NEq _ -> NEq
        | _ -> failwith "get_cond should only be called for comparison expressions"
    in

    match node.node with
    | Ast.String s -> StringLiteral s |> singleton
    | Ast.Int i -> Mov (Reg RAX, Imm i) |> singleton
    | Ast.Bool b -> Mov (Reg RAX, if b then Imm 1 else Imm 0) |> singleton
    | Ast.Nullptr -> Mov (Reg RAX, Imm 0) |> singleton
    | Ast.Variable (name, index_expr) -> (
        let info = get_symbol_info name ctx in
        match index_expr with
        | None -> Mov (Reg RAX, Mem (RBP, info.offset)) |> singleton
        | Some e ->
            push_back
              (Mov (Reg RAX, Weirdo (info.offset, RAX, info.element_size)))
              (generate_expression ctx e))
    | Ast.Add (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Pop (Reg RBX); Add (Reg RAX, Reg RBX) ]
    | Ast.Sub (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Mov (Reg RBX, Reg RAX); Pop (Reg RAX); Sub (Reg RAX, Reg RBX) ]
    | Ast.Mul (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Pop (Reg RBX); IMul (Reg RAX, Reg RBX) ]
    | Ast.Div (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Mov (Reg RBX, Reg RAX); Pop (Reg RAX); Div (Reg RBX) ]
    | Ast.Lsh (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Mov (Reg RCX, Reg RAX); Pop (Reg RAX); Lsh (Reg RAX, Reg RCX) ]
    | Ast.Rsh (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Mov (Reg RCX, Reg RAX); Pop (Reg RAX); Rsh (Reg RAX, Reg RCX) ]
    | Ast.BAnd (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Pop (Reg RBX); And (Reg RAX, Reg RBX) ]
    | Ast.BOr (lhs, rhs) ->
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        @ of_list [ Pop (Reg RBX); Or (Reg RAX, Reg RBX) ]
    | Ast.Gt (lhs, rhs)
    | Ast.GEq (lhs, rhs)
    | Ast.Lt (lhs, rhs)
    | Ast.LEq (lhs, rhs)
    | Ast.Eq (lhs, rhs)
    | Ast.NEq (lhs, rhs) ->
        let cond = get_cond node.node in
        generate_expression ctx lhs
        @ singleton (Push (Reg RAX))
        @ generate_expression ctx rhs
        (* note the order in cmp, since rhs is in rax and lhs in rbx *)
        @ of_list [ Pop (Reg RBX); Cmp (Reg RBX, Reg RAX); Set (cond, Reg RAX) ]
    | Ast.UNot e -> generate_expression ctx e @ singleton (Not (Reg RAX))
    | Ast.VarAssign (name, expr) ->
        let info = get_symbol_info name ctx in
        generate_expression ctx expr @ (Mov (Mem (RBP, info.offset), Reg RAX) |> singleton)
    | Ast.ArrayVarAssign (name, index_expr, expr) ->
        let info = get_symbol_info name ctx in
        generate_expression ctx index_expr
        @ (Push (Reg RAX) |> singleton)
        @ generate_expression ctx expr
        @ of_list [ Pop (Reg RBX); Mov (Weirdo (info.offset, RBX, info.element_size), Reg RAX) ]
    | Ast.IfElse (cond, body, else_body) -> (
        let else_label = get_label ctx in
        generate_expression ctx cond
        @ of_list [ Test (Reg RAX, Reg RAX); Jmp_cond (Z, else_label) ]
        @ generate_expression ctx body
        @
        match else_body with
        | None -> singleton (Label else_label)
        | Some e ->
            let end_label = get_label ctx in
            of_list [ Jmp end_label; Label else_label ]
            @ generate_expression ctx e
            @ singleton (Label end_label))
    | Ast.Block (stmt_list, e) -> (
        let new_ctx = { ctx with parent = Some ctx; symbols = H.create () } in
        let new_ctx, instrs =
            List.fold stmt_list
              ~init:(new_ctx, empty ())
              ~f:(fun (ctx, l) s ->
                let ctx', l' = generate_statement s ctx in
                (ctx', append l l'))
        in
        match e with
        | None -> instrs
        | Some e -> instrs @ generate_expression new_ctx e)
    | Ast.FnCall (name, args) ->
        let _, arg_instrs, arg_loads =
            List.foldi args
              ~init:(ctx, empty (), empty ())
              ~f:(fun i (ctx, instrs, arg_loads) arg ->
                let format_arg = sprintf "__%s_arg_%d__" name i in
                let dummy_node =
                    { loc = arg.loc; node = Ast.Declaration_assign (format_arg, Type "int", arg) }
                in
                let ctx, l = generate_statement dummy_node ctx in
                let info = get_symbol_info format_arg ctx in
                let reg = get_fn_arg_register i in
                (ctx, instrs @ l, push_back (Mov (Reg reg, Mem (RBP, info.offset))) arg_loads))
        in
        arg_instrs @ arg_loads @ singleton (Call name)

let gen ast =
    let ctx0 =
        { parent = None; symbols = H.create (); current_offset = 0; current_label = ref 0 }
    in
    let _, functions, instrs =
        List.fold ast
          ~init:(ctx0, empty (), empty ())
          ~f:(fun (ctx, functions, instrs) s ->
            match s.node with
            | FnDeclaration _ ->
                let ctx', l = generate_statement s ctx in
                (ctx', append functions l, instrs)
            | _ ->
                let ctx', l = generate_statement s ctx in
                (ctx', functions, append instrs l))
    in
    (functions, instrs) |> Tuple2.map ~f:to_list
