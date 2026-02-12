type level =
    | Debug
    | Info
    | Warning
    | Error

let color_of_level = function
    | Debug -> "\027[36m"
    | Info -> "\027[92m"
    | Warning -> "\027[33m"
    | Error -> "\027[31m"

let reset_color = "\027[0m"

let pp_level ppf = function
    | Error -> Format.pp_print_string ppf "ERROR"
    | Warning -> Format.pp_print_string ppf "WARNING"
    | Info -> Format.pp_print_string ppf "INFO"
    | Debug -> Format.pp_print_string ppf "DEBUG"

let env_var_log_level =
    match Sys.getenv_opt "DAWN_LOG_LEVEL" with
    | Some "debug" -> Some Debug
    | Some "info" -> Some Info
    | Some "warn" -> Some Warning
    | Some "error" -> Some Error
    | Some other ->
        Printf.eprintf "Warning: Unknown log level '%s'\n%!" other;
        None
    | None -> None

let cur_level =
    match env_var_log_level with
    | None -> ref Warning
    | Some l -> ref l

let set_level l =
    match env_var_log_level with
    | None -> cur_level := l
    | Some _ -> (* env var ovewrites always *) ()

let should_log level = level >= !cur_level

let log ?loc_str level k =
    if should_log level then
      k (fun fmt ->
          Format.printf "%s[%a]:%s " (color_of_level level) pp_level level reset_color;
          (match loc_str with
          | None -> ()
          | Some loc -> Format.printf "%s(%s)%s " (color_of_level level) loc reset_color);
          Format.kfprintf (fun ppf -> Format.pp_print_newline ppf ()) Format.std_formatter fmt)
    else
      ()

let log_s ?loc_str level s =
    if should_log level then
      match
        loc_str
      with
      | None -> Format.printf "%s[%a]:%s %s" (color_of_level level) pp_level level reset_color s
      | Some loc ->
          Format.printf "%s[%a]: (%s)%s %s" (color_of_level level) pp_level level loc reset_color s

let log_debug ?loc_str f = log ?loc_str Debug f
let log_info ?loc_str f = log ?loc_str Info f
let log_warn ?loc_str f = log ?loc_str Warning f
let log_error ?loc_str f = log ?loc_str Error f
let log_debug_s ?loc_str s = log_s ?loc_str Debug s
let log_info_s ?loc_str s = log_s ?loc_str Info s
let log_warn_s ?loc_str s = log_s ?loc_str Warning s
let log_error_s ?loc_str s = log_s ?loc_str Error s
