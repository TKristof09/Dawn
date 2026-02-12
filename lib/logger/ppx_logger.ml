open Ppxlib

let expand_log level ~ctxt expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let pos = loc.loc_start in
    let filename = pos.pos_fname in
    let line = pos.pos_lnum in

    (* Extract module name from filename *)
    let module_name =
        Filename.basename filename |> Filename.remove_extension |> String.capitalize_ascii
    in
    let loc_str = Ast_builder.Default.estring ~loc (Printf.sprintf "%s:%d" module_name line) in
    match expr.pexp_desc with
    | Pexp_apply (({ pexp_desc = Pexp_constant (Pconst_string _); _ } as fmt_expr), args) ->
        (* Format string with args *)
        let m_var = Ast_builder.Default.evar ~loc "m" in
        let m_applied = Ast_builder.Default.pexp_apply ~loc m_var ((Nolabel, fmt_expr) :: args) in
        let level_expr = Ast_builder.Default.evar ~loc ("Logger.log_" ^ level) in
        (* need the dummy _ type annotation on m because otherwise the lsp tries to put inlay hint for the type name and it can sometimes be like 3 lines long *)
        [%expr [%e level_expr] ~loc_str:[%e loc_str] (fun (m : _) -> [%e m_applied])]
    | _ ->
        (* Arbitrary expression returning string *)
        let level_expr = Ast_builder.Default.evar ~loc ("Logger.log_" ^ level ^ "_s") in
        [%expr [%e level_expr] ~loc_str:[%e loc_str] [%e expr]]

let declare_extension level =
    Extension.V3.declare ("log." ^ level) Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (expand_log level)

let () =
    Driver.register_transformation
      ~rules:
        [
          Context_free.Rule.extension (declare_extension "debug");
          Context_free.Rule.extension (declare_extension "info");
          Context_free.Rule.extension (declare_extension "warn");
          Context_free.Rule.extension (declare_extension "error");
        ]
      "ppx_logger"
