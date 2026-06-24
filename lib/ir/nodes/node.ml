open Core

module N = struct
  type data = [ `Data ]
  and ctrl = [ `Ctrl ]
  and mem = [ `Mem ]
  and misc = [ `Misc ] [@@deriving sexp_of]

  type any = AnyNode : ('a, 'tag) t -> any
  and any_data = AnyData : ('a, data) t -> any_data
  and any_ctrl = AnyCtrl : ('a, ctrl) t -> any_ctrl
  and any_mem = AnyMem : ('a, mem) t -> any_mem

  and _ data_kind =
      | Constant : unit data_kind
      | Add : binop data_kind
      | Sub : binop data_kind
      | Mul : binop data_kind
      | Div : binop data_kind
      | Lsh : binop data_kind
      | Rsh : binop data_kind
      | BAnd : binop data_kind
      | BOr : binop data_kind
      | Eq : binop data_kind
      | NEq : binop data_kind
      | Lt : binop data_kind
      | LEq : binop data_kind
      | Gt : binop data_kind
      | GEq : binop data_kind
      | Phi : any_data phi data_kind
      | Proj : int -> any unary data_kind
      | Param : int -> any_data phi data_kind
      | External : string -> extern_fun data_kind
      | Cast : any_data unary data_kind
      | Load : string -> load data_kind
      | AddrOf : addr_of data_kind
      | AddrOfField : string -> addr_of data_kind
      | Deref : deref data_kind

  and _ ctrl_kind =
      | Start : unit ctrl_kind
      | Stop : stop ctrl_kind
      | Proj : int -> any unary ctrl_kind
      | If : any_data unary ctrl_kind
      | Region : merge_point ctrl_kind
      | Loop : loop ctrl_kind
      | Function : {
          ret : (ret, ctrl) t;
          signature : (Types.t[@sexp.opaque]);
          idx : int;
        }
          -> fun_def ctrl_kind
      | Return : ret ctrl_kind
      | FunctionCall : fun_call ctrl_kind
      | FunctionCallEnd : fun_call_end ctrl_kind

  and _ mem_kind =
      | New : alloc mem_kind
      | Store : string -> store mem_kind
      | Copy : copy mem_kind
      | Phi : any_mem phi mem_kind
      | Param : any_mem phi mem_kind
      | Proj : int -> any unary mem_kind

  and ('a, 'tag) kind =
      | Data : 'a data_kind -> ('a, data) kind
      | Ctrl : 'a ctrl_kind -> ('a, ctrl) kind
      | Mem : 'a mem_kind -> ('a, mem) kind
      | Scope : any Symbol_table.t -> (scope_kind, misc) kind
      | ForwardRef : string -> (unit, data) kind

  and binop = {
      lhs : any_data option;
      rhs : any_data option;
    }

  and 'a unary = { input : 'a option }
  and 'a phi = { phi_inputs : 'a option list }
  and stop = { mem : any_mem option }
  and merge_point = { ctrl_inputs : any_ctrl option list }

  and loop = {
      entry : any_ctrl option;
      backedge : any_ctrl option;
    }

  and fun_def = { call_sites : any_ctrl option list }
  and extern_fun = { params : any_data list }

  and ret = {
      mem : any_mem option;
      data : any_data option;
    }

  and fun_call = {
      fun_ptr : any_data option;
      mem : any_mem option;
      args : any_data option list;
    }

  and fun_call_end = { ret_nodes : any_ctrl option list }

  and alloc = {
      mem : any_mem option;
      size : any_data option;
    }

  and load = {
      mem : any_mem option;
      ptr : any_data option;
    }

  and store = {
      mem : any_mem option;
      ptr : any_data option;
      value : any_data option;
    }

  and addr_of = {
      place : any_data option;
      offset : any_data option;
    }

  and deref = {
      mem : any_mem option;
      ptr : any_data option;
    }

  and copy = {
      mem : any_mem option;
      src : any_data option;
      dst : any_data option;
    }

  and scope_kind = { vars : any option list }

  and ('a, 'tag) t = {
      mutable typ : (Types.t[@sexp.opaque]);
      mutable min_typ : (Types.t option[@sexp.opaque]);
      mutable kind : ('a, 'tag) kind;
      id : int;
      loc : Ast.loc;
      mutable parent_fun : int option;
      list_of_inputs : ('a -> any option list[@sexp.opaque]);
      inputs_of_list : (any option list -> 'a[@sexp.opaque]);
    }
  [@@deriving sexp_of]

  let id_counter = ref 0
  let reset_id () = id_counter := 0

  let next_id () =
      incr id_counter;
      !id_counter

  let[@inline] any_of_data n =
      match n with
      | None -> None
      | Some (AnyData n) -> Some (AnyNode n)

  let[@inline] any_of_ctrl n =
      match n with
      | None -> None
      | Some (AnyCtrl n) -> Some (AnyNode n)

  let[@inline] any_of_mem n =
      match n with
      | None -> None
      | Some (AnyMem n) -> Some (AnyNode n)

  let[@inline] data_of_any (n : any option) : any_data option =
      match n with
      | None -> None
      | Some (AnyNode n) -> (
          match n.kind with
          | Data _ -> Some (AnyData n)
          | ForwardRef _ -> Some (AnyData n)
          | _ -> None)

  let[@inline] ctrl_of_any (n : any option) : any_ctrl option =
      match n with
      | None -> None
      | Some (AnyNode n) -> (
          match n.kind with
          | Ctrl _ -> Some (AnyCtrl n)
          | _ -> None)

  let[@inline] mem_of_any (n : any option) : any_mem option =
      match n with
      | None -> None
      | Some (AnyNode n) -> (
          match n.kind with
          | Mem _ -> Some (AnyMem n)
          | _ -> None)

  let get_list_of_inputs : type a tag. (a, tag) kind -> a -> any option list =
     fun k ->
      let binop_inputs = fun { lhs; rhs } -> [ any_of_data lhs; any_of_data rhs ] in
      match k with
      | Data Constant -> Fun.const []
      | Data Add -> binop_inputs
      | Data Sub -> binop_inputs
      | Data Mul -> binop_inputs
      | Data Div -> binop_inputs
      | Data Lsh -> binop_inputs
      | Data Rsh -> binop_inputs
      | Data BAnd -> binop_inputs
      | Data BOr -> binop_inputs
      | Data Eq -> binop_inputs
      | Data NEq -> binop_inputs
      | Data Lt -> binop_inputs
      | Data LEq -> binop_inputs
      | Data Gt -> binop_inputs
      | Data GEq -> binop_inputs
      | Data (Proj _) -> fun { input } -> [ input ]
      | Data Phi -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_data
      | Data (Param _) -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_data
      | Data (External _) ->
          fun { params } -> List.map params ~f:Option.some |> List.map ~f:any_of_data
      | Data Cast -> fun { input } -> [ any_of_data input ]
      | Data (Load _) -> fun { mem; ptr } -> [ any_of_mem mem; any_of_data ptr ]
      | Data AddrOf -> fun { place; offset } -> [ any_of_data place; any_of_data offset ]
      | Data (AddrOfField _) -> fun { place; offset } -> [ any_of_data place; any_of_data offset ]
      | Data Deref -> fun { mem; ptr } -> [ any_of_mem mem; any_of_data ptr ]
      | Ctrl Start -> fun () -> []
      | Ctrl Stop -> fun { mem } -> [ any_of_mem mem ]
      | Ctrl (Proj _) -> fun { input } -> [ input ]
      | Ctrl If -> fun { input } -> [ any_of_data input ]
      | Ctrl Region -> fun { ctrl_inputs } -> List.map ctrl_inputs ~f:any_of_ctrl
      | Ctrl Loop -> fun { entry; backedge } -> [ any_of_ctrl entry; any_of_ctrl backedge ]
      | Ctrl (Function _) -> fun { call_sites } -> List.map call_sites ~f:any_of_ctrl
      | Ctrl Return -> fun { mem; data } -> [ any_of_mem mem; any_of_data data ]
      | Ctrl FunctionCall ->
          fun { fun_ptr; mem; args } ->
            any_of_data fun_ptr :: any_of_mem mem :: List.map args ~f:any_of_data
      | Ctrl FunctionCallEnd -> fun { ret_nodes } -> List.map ret_nodes ~f:any_of_ctrl
      | Mem New -> fun { mem; size } -> [ any_of_mem mem; any_of_data size ]
      | Mem (Store _) ->
          fun { mem; ptr; value } -> [ any_of_mem mem; any_of_data ptr; any_of_data value ]
      | Mem Copy -> fun { mem; src; dst } -> [ any_of_mem mem; any_of_data src; any_of_data dst ]
      | Mem Phi -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_mem
      | Mem Param -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_mem
      | Mem (Proj _) -> fun { input } -> [ input ]
      | Scope _ -> fun { vars } -> vars
      | ForwardRef _ -> fun () -> []

  let get_inputs_of_list : type a tag. (a, tag) kind -> any option list -> a =
     fun k ->
      let binop_inputs = function
          | [ x; y ] -> { lhs = data_of_any x; rhs = data_of_any y }
          | _ -> assert false
      in
      match k with
      | Data Constant -> Fun.const ()
      | Data Add -> binop_inputs
      | Data Sub -> binop_inputs
      | Data Mul -> binop_inputs
      | Data Div -> binop_inputs
      | Data Lsh -> binop_inputs
      | Data Rsh -> binop_inputs
      | Data BAnd -> binop_inputs
      | Data BOr -> binop_inputs
      | Data Eq -> binop_inputs
      | Data NEq -> binop_inputs
      | Data Lt -> binop_inputs
      | Data LEq -> binop_inputs
      | Data Gt -> binop_inputs
      | Data GEq -> binop_inputs
      | Data (Proj _) -> (
          function
          | [ x ] -> { input = x }
          | _ -> assert false)
      | Data Phi -> fun lst -> { phi_inputs = List.map lst ~f:data_of_any }
      | Data (Param _) -> fun lst -> { phi_inputs = List.map lst ~f:data_of_any }
      | Data (External _) ->
          fun lst -> { params = List.map lst ~f:(fun o -> data_of_any o |> Option.value_exn) }
      | Data Cast -> (
          function
          | [ x ] -> { input = data_of_any x }
          | _ -> assert false)
      | Data (Load _) -> (
          function
          | [ x; y ] -> { mem = mem_of_any x; ptr = data_of_any y }
          | _ -> assert false)
      | Data AddrOf -> (
          function
          | [ x; y ] -> { place = data_of_any x; offset = data_of_any y }
          | _ -> assert false)
      | Data (AddrOfField _) -> (
          function
          | [ x; y ] -> { place = data_of_any x; offset = data_of_any y }
          | _ -> assert false)
      | Data Deref -> (
          function
          | [ x; y ] -> { mem = mem_of_any x; ptr = data_of_any y }
          | _ -> assert false)
      | Ctrl Start -> Fun.const ()
      | Ctrl Stop -> (
          function
          | [ x ] -> { mem = mem_of_any x }
          | _ -> assert false)
      | Ctrl (Proj _) -> (
          function
          | [ x ] -> { input = x }
          | _ -> assert false)
      | Ctrl If -> (
          function
          | [ x ] -> { input = data_of_any x }
          | _ -> assert false)
      | Ctrl Region -> fun lst -> { ctrl_inputs = List.map lst ~f:ctrl_of_any }
      | Ctrl Loop -> (
          function
          | [ x; y ] -> { entry = ctrl_of_any x; backedge = ctrl_of_any y }
          | _ -> assert false)
      | Ctrl (Function _) -> fun lst -> { call_sites = List.map lst ~f:ctrl_of_any }
      | Ctrl Return -> (
          function
          | [ x; y ] -> { mem = mem_of_any x; data = data_of_any y }
          | _ -> assert false)
      | Ctrl FunctionCall -> (
          function
          | x :: y :: rest ->
              { fun_ptr = data_of_any x; mem = mem_of_any y; args = List.map rest ~f:data_of_any }
          | _ -> assert false)
      | Ctrl FunctionCallEnd -> fun lst -> { ret_nodes = List.map lst ~f:ctrl_of_any }
      | Mem New -> (
          function
          | [ x; y ] -> { mem = mem_of_any x; size = data_of_any y }
          | _ -> assert false)
      | Mem (Store _) -> (
          function
          | [ x; y; z ] -> { mem = mem_of_any x; ptr = data_of_any y; value = data_of_any z }
          | _ -> assert false)
      | Mem Copy -> (
          function
          | [ x; y; z ] -> { mem = mem_of_any x; src = data_of_any y; dst = data_of_any z }
          | _ -> assert false)
      | Mem Phi -> fun lst -> { phi_inputs = List.map lst ~f:mem_of_any }
      | Mem Param -> fun lst -> { phi_inputs = List.map lst ~f:mem_of_any }
      | Mem (Proj _) -> (
          function
          | [ x ] -> { input = x }
          | _ -> assert false)
      | Scope _ -> fun lst -> { vars = lst }
      | ForwardRef _ -> Fun.const ()

  let create_data ?parent_fun loc typ kind =
      {
        typ;
        min_typ = None;
        kind = Data kind;
        id = next_id ();
        loc;
        parent_fun;
        list_of_inputs = get_list_of_inputs (Data kind);
        inputs_of_list = get_inputs_of_list (Data kind);
      }

  let create_ctrl ?parent_fun loc typ kind =
      {
        typ;
        min_typ = None;
        kind = Ctrl kind;
        id = next_id ();
        loc;
        parent_fun;
        list_of_inputs = get_list_of_inputs (Ctrl kind);
        inputs_of_list = get_inputs_of_list (Ctrl kind);
      }

  let create_mem ?parent_fun loc typ kind =
      {
        typ;
        min_typ = None;
        kind = Mem kind;
        id = next_id ();
        loc;
        parent_fun;
        list_of_inputs = get_list_of_inputs (Mem kind);
        inputs_of_list = get_inputs_of_list (Mem kind);
      }

  let create_scope () =
      let kind = Scope (Symbol_table.create ()) in
      {
        typ = Types.ALL;
        min_typ = None;
        kind;
        id = next_id ();
        loc = { filename = ""; line = 0; col = 0 };
        parent_fun = None;
        list_of_inputs = get_list_of_inputs kind;
        inputs_of_list = get_inputs_of_list kind;
      }

  let create_forward_ref name =
      {
        typ = Types.ALL;
        min_typ = None;
        kind = ForwardRef name;
        id = next_id ();
        loc = { filename = ""; line = 0; col = 0 };
        parent_fun = None;
        list_of_inputs = get_list_of_inputs (ForwardRef name);
        inputs_of_list = get_inputs_of_list (ForwardRef name);
      }

  let id n = n.id
  let equal a b = a.id = b.id
  let list_of_inputs n inps = n.list_of_inputs inps
  let inputs_of_list n l = n.inputs_of_list l

  let rec pp_data_kind : type a. Format.formatter -> a data_kind -> unit =
     fun fmt -> function
      | Constant -> Format.pp_print_string fmt "Constant"
      | Add -> Format.pp_print_string fmt "Add"
      | Sub -> Format.pp_print_string fmt "Sub"
      | Mul -> Format.pp_print_string fmt "Mul"
      | Div -> Format.pp_print_string fmt "Div"
      | Lsh -> Format.pp_print_string fmt "Lsh"
      | Rsh -> Format.pp_print_string fmt "Rsh"
      | BAnd -> Format.pp_print_string fmt "BAnd"
      | BOr -> Format.pp_print_string fmt "BOr"
      | Eq -> Format.pp_print_string fmt "Eq"
      | NEq -> Format.pp_print_string fmt "NEq"
      | Lt -> Format.pp_print_string fmt "Lt"
      | LEq -> Format.pp_print_string fmt "LEq"
      | Gt -> Format.pp_print_string fmt "Gt"
      | GEq -> Format.pp_print_string fmt "GEq"
      | Phi -> Format.pp_print_string fmt "Phi"
      | Proj i -> Format.fprintf fmt "Proj %d" i
      | Param i -> Format.fprintf fmt "Param %d" i
      | External s -> Format.fprintf fmt "External %s" s
      | Cast -> Format.pp_print_string fmt "Cast"
      | Load s -> Format.fprintf fmt "Load %s" s
      | AddrOf -> Format.pp_print_string fmt "AddrOf"
      | AddrOfField s -> Format.fprintf fmt "AddrOfField %s" s
      | Deref -> Format.pp_print_string fmt "Deref"

  and pp_ctrl_kind : type a. Format.formatter -> a ctrl_kind -> unit =
     fun fmt -> function
      | Start -> Format.pp_print_string fmt "Start"
      | Stop -> Format.pp_print_string fmt "Stop"
      | Proj i -> Format.fprintf fmt "Proj %d" i
      | If -> Format.pp_print_string fmt "If"
      | Region -> Format.pp_print_string fmt "Region"
      | Loop -> Format.pp_print_string fmt "Loop"
      | Function { ret = _; signature = _; idx } -> Format.fprintf fmt "Function %d" idx
      | Return -> Format.pp_print_string fmt "Return"
      | FunctionCall -> Format.pp_print_string fmt "FunctionCall"
      | FunctionCallEnd -> Format.pp_print_string fmt "FunctionCallEnd"

  and pp_mem_kind : type a. Format.formatter -> a mem_kind -> unit =
     fun fmt -> function
      | New -> Format.pp_print_string fmt "New"
      | Store s -> Format.fprintf fmt "Store %s" s
      | Copy -> Format.pp_print_string fmt "Copy"
      | Phi -> Format.pp_print_string fmt "Phi"
      | Param -> Format.pp_print_string fmt "Param"
      | Proj i -> Format.fprintf fmt "Proj %d" i

  and pp_kind : type a b. Format.formatter -> (a, b) kind -> unit =
     fun fmt -> function
      | Data k -> Format.fprintf fmt "Data(%a)" pp_data_kind k
      | Ctrl k -> Format.fprintf fmt "Ctrl(%a)" pp_ctrl_kind k
      | Mem k -> Format.fprintf fmt "Mem(%a)" pp_mem_kind k
      | Scope _ -> Format.pp_print_string fmt "Scope"
      | ForwardRef s -> Format.fprintf fmt "ForwardRef %s" s

  and pp fmt n = Format.fprintf fmt "(node #%d %a)" n.id pp_kind n.kind
  and pp_any fmt (AnyNode n) = Format.fprintf fmt "(node #%d %a)" n.id pp_kind n.kind
  and show n = Format.asprintf "%a" pp n
  and show_kind k = Format.asprintf "%a" pp_kind k

  let[@inline] kind_eq : type a b taga tagb.
      (a, taga) kind -> (b, tagb) kind -> ((a, b) Type.eq * (taga, tagb) Type.eq) option =
     fun a b ->
      match (a, b) with
      (* Data *)
      | Data Constant, Data Constant -> Some (Type.Equal, Type.Equal)
      | Data Add, Data Add -> Some (Type.Equal, Type.Equal)
      | Data Sub, Data Sub -> Some (Type.Equal, Type.Equal)
      | Data Mul, Data Mul -> Some (Type.Equal, Type.Equal)
      | Data Div, Data Div -> Some (Type.Equal, Type.Equal)
      | Data Lsh, Data Lsh -> Some (Type.Equal, Type.Equal)
      | Data Rsh, Data Rsh -> Some (Type.Equal, Type.Equal)
      | Data BAnd, Data BAnd -> Some (Type.Equal, Type.Equal)
      | Data BOr, Data BOr -> Some (Type.Equal, Type.Equal)
      | Data Eq, Data Eq -> Some (Type.Equal, Type.Equal)
      | Data NEq, Data NEq -> Some (Type.Equal, Type.Equal)
      | Data Lt, Data Lt -> Some (Type.Equal, Type.Equal)
      | Data LEq, Data LEq -> Some (Type.Equal, Type.Equal)
      | Data Gt, Data Gt -> Some (Type.Equal, Type.Equal)
      | Data GEq, Data GEq -> Some (Type.Equal, Type.Equal)
      | Data Phi, Data Phi -> Some (Type.Equal, Type.Equal)
      | Data (External _), Data (External _) -> Some (Type.Equal, Type.Equal)
      | Data Cast, Data Cast -> Some (Type.Equal, Type.Equal)
      | Data (Proj _), Data (Proj _) -> Some (Type.Equal, Type.Equal)
      | Data (Param _), Data (Param _) -> Some (Type.Equal, Type.Equal)
      | Data AddrOf, Data AddrOf -> Some (Type.Equal, Type.Equal)
      | Data Deref, Data Deref -> Some (Type.Equal, Type.Equal)
      | Data (Load _), Data (Load _) -> Some (Type.Equal, Type.Equal)
      | Data (AddrOfField _), Data (AddrOfField _) -> Some (Type.Equal, Type.Equal)
      (* Ctrl *)
      | Ctrl Start, Ctrl Start -> Some (Type.Equal, Type.Equal)
      | Ctrl Stop, Ctrl Stop -> Some (Type.Equal, Type.Equal)
      | Ctrl If, Ctrl If -> Some (Type.Equal, Type.Equal)
      | Ctrl Region, Ctrl Region -> Some (Type.Equal, Type.Equal)
      | Ctrl Loop, Ctrl Loop -> Some (Type.Equal, Type.Equal)
      | Ctrl Return, Ctrl Return -> Some (Type.Equal, Type.Equal)
      | Ctrl FunctionCall, Ctrl FunctionCall -> Some (Type.Equal, Type.Equal)
      | Ctrl FunctionCallEnd, Ctrl FunctionCallEnd -> Some (Type.Equal, Type.Equal)
      | Ctrl (Proj _), Ctrl (Proj _) -> Some (Type.Equal, Type.Equal)
      | Ctrl (Function _), Ctrl (Function _) -> Some (Type.Equal, Type.Equal)
      (* Mem *)
      | Mem New, Mem New -> Some (Type.Equal, Type.Equal)
      | Mem Copy, Mem Copy -> Some (Type.Equal, Type.Equal)
      | Mem (Store _), Mem (Store _) -> Some (Type.Equal, Type.Equal)
      | Mem (Proj _), Mem (Proj _) -> Some (Type.Equal, Type.Equal)
      | Mem Phi, Mem Phi -> Some (Type.Equal, Type.Equal)
      | Mem Param, Mem Param -> Some (Type.Equal, Type.Equal)
      (* Misc *)
      | Scope _, Scope _ -> Some (Type.Equal, Type.Equal)
      | ForwardRef _, ForwardRef _ -> Some (Type.Equal, Type.Equal)
      (* Data *)
      | Data Constant, _ -> None
      | Data Add, _ -> None
      | Data Sub, _ -> None
      | Data Mul, _ -> None
      | Data Div, _ -> None
      | Data Lsh, _ -> None
      | Data Rsh, _ -> None
      | Data BAnd, _ -> None
      | Data BOr, _ -> None
      | Data Eq, _ -> None
      | Data NEq, _ -> None
      | Data Lt, _ -> None
      | Data LEq, _ -> None
      | Data Gt, _ -> None
      | Data GEq, _ -> None
      | Data Phi, _ -> None
      | Data (External _), _ -> None
      | Data Cast, _ -> None
      | Data (Proj _), _ -> None
      | Data (Param _), _ -> None
      | Data AddrOf, _ -> None
      | Data Deref, _ -> None
      | Data (Load _), _ -> None
      | Data (AddrOfField _), _ -> None
      (* Ctrl *)
      | Ctrl Start, _ -> None
      | Ctrl Stop, _ -> None
      | Ctrl If, _ -> None
      | Ctrl Region, _ -> None
      | Ctrl Loop, _ -> None
      | Ctrl Return, _ -> None
      | Ctrl FunctionCall, _ -> None
      | Ctrl FunctionCallEnd, _ -> None
      | Ctrl (Proj _), _ -> None
      | Ctrl (Function _), _ -> None
      (* Mem *)
      | Mem New, _ -> None
      | Mem Copy, _ -> None
      | Mem (Store _), _ -> None
      | Mem (Proj _), _ -> None
      | Mem Phi, _ -> None
      | Mem Param, _ -> None
      (* Misc *)
      | Scope _, _ -> None
      | ForwardRef _, _ -> None

  let[@inline] type_eq : type a b taga tagb.
      (a, taga) t -> (b, tagb) t -> ((a, b) Type.eq * (taga, tagb) Type.eq) option =
     fun a b -> kind_eq a.kind b.kind

  let[@inline] unpack : type a b c d. (a, c) t -> (b, d) kind -> (b, d) t option =
     fun n k ->
      match kind_eq n.kind k with
      | Some (Type.Equal, Type.Equal) -> Some n
      | None -> None

  let[@inline] unpack_exn : type a b c d. (a, c) t -> (b, d) kind -> (b, d) t =
     fun n k -> unpack n k |> Option.value_exn

  let[@inline] as_ctrl_exn : type a b. (a, b) t -> (a, ctrl) t =
     fun n ->
      match n.kind with
      | Ctrl _ -> n
      | _ -> failwithf "Not a control node %s" (show n) ()

  let[@inline] as_data_exn : type a b. (a, b) t -> (a, data) t =
     fun n ->
      match n.kind with
      | Data _ -> n
      | ForwardRef _ -> n
      | _ -> failwithf "Not a data node %s" (show n) ()

  let[@inline] as_mem_exn : type a b. (a, b) t -> (a, mem) t =
     fun n ->
      match n.kind with
      | Mem _ -> n
      | _ -> failwithf "Not a memory node %s" (show n) ()

  let is_ctrl : type a b. (a, b) t -> bool =
     fun n ->
      match n.kind with
      | Ctrl _ -> true
      | _ -> false

  let is_data : type a b. (a, b) t -> bool =
     fun n ->
      match n.kind with
      | Data _ -> true
      | _ -> false

  let is_blockhead : type a b. (a, b) t -> bool =
     fun n ->
      match n.kind with
      | Ctrl Start
      | Ctrl (Proj _)
      | Ctrl Region
      | Ctrl Loop
      | Ctrl Stop ->
          true
      | _ -> false
end

include N
module G = Graph.Make (N)

module Any = struct
  type t = any

  let compare (AnyNode a) (AnyNode b) = Int.compare a.id b.id
  let equal (AnyNode a) (AnyNode b) = Int.equal a.id b.id
  let hash (AnyNode a) = Int.hash a.id
  let sexp_of_t = sexp_of_any
end

module AnyData = struct
  type t = any_data

  let compare (AnyData a) (AnyData b) = Int.compare a.id b.id
  let equal (AnyData a) (AnyData b) = Int.equal a.id b.id
  let hash (AnyData a) = Int.hash a.id
  let sexp_of_t = sexp_of_any_data
end

module AnyCtrl = struct
  type t = any_ctrl

  let compare (AnyCtrl a) (AnyCtrl b) = Int.compare a.id b.id
  let equal (AnyCtrl a) (AnyCtrl b) = Int.equal a.id b.id
  let hash (AnyCtrl a) = Int.hash a.id
  let sexp_of_t = sexp_of_any_ctrl
end

module AnyMem = struct
  type t = any_mem

  let compare (AnyMem a) (AnyMem b) = Int.compare a.id b.id
  let equal (AnyMem a) (AnyMem b) = Int.equal a.id b.id
  let hash (AnyMem a) = Int.hash a.id
  let sexp_of_t = sexp_of_any_mem
end
