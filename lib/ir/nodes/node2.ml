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
      | Proj : int -> any_data unary data_kind
      | Param : int -> any_data phi data_kind
      | External : string -> unit data_kind
      | Cast : any_data unary data_kind

  and _ ctrl_kind =
      | Start : unit ctrl_kind
      | Stop : stop ctrl_kind
      | Proj : int -> any_ctrl unary ctrl_kind
      | If : branch ctrl_kind
      | Region : merge_point ctrl_kind
      | Loop : loop ctrl_kind
      | Function : {
          ret : (ret, ctrl) t;
          signature : Types.t;
          idx : int;
        }
          -> fun_def ctrl_kind
      | Return : ret ctrl_kind
      | FunctionCall : fun_call ctrl_kind
      | FunctionCallEnd : fun_call_end ctrl_kind

  and _ mem_kind =
      | New : alloc mem_kind
      | Load : string -> load mem_kind
      | Store : string -> store mem_kind
      | AddrOf : addr_of mem_kind
      | AddrOfField : string -> addr_of mem_kind
      | Deref : deref mem_kind
      | Copy : copy mem_kind
      | Phi : any_mem phi mem_kind
      | Param : any_mem phi mem_kind

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

  and 'a unary = { inp : 'a option }
  and 'a phi = { phi_inputs : 'a option list }
  and stop = { mem : any_mem option }

  and branch = {
      cond : any_data option;
      true_branch : any_ctrl option;
      false_branch : any_ctrl option;
    }

  and merge_point = { ctrl_inputs : any_ctrl option list }

  and loop = {
      entry : any_ctrl option;
      backedge : any_ctrl option;
    }

  and fun_def = { call_sites : any_ctrl option list }

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
      ptr : any_mem option;
    }

  and store = {
      mem : any_mem option;
      ptr : any_mem option;
      value : any_data option;
    }

  and addr_of = {
      place : any_data option;
      offset : any_data option;
    }

  and deref = {
      mem : any_mem option;
      ptr : any_mem option;
    }

  and copy = {
      mem : any_mem option;
      src : any_mem option;
      dst : any_mem option;
    }

  and scope_kind = { vars : any option list }

  and ('a, 'tag) t = {
      mutable typ : Types.t;
      mutable min_typ : Types.t option;
      mutable kind : ('a, 'tag) kind;
      id : int;
      loc : Ast.loc;
      mutable parent_fun : int option;
      list_of_inputs : 'a -> any option list;
      inputs_of_list : any option list -> 'a;
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
      | Data (Proj _) -> fun { inp } -> [ any_of_data inp ]
      | Data Phi -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_data
      | Data (Param _) -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_data
      | Data (External _) -> Fun.const []
      | Data Cast -> fun { inp } -> [ any_of_data inp ]
      | Ctrl Start -> fun () -> []
      | Ctrl Stop -> fun { mem } -> [ any_of_mem mem ]
      | Ctrl (Proj _) -> fun { inp } -> [ any_of_ctrl inp ]
      | Ctrl If ->
          fun { cond; true_branch; false_branch } ->
            [ any_of_data cond; any_of_ctrl true_branch; any_of_ctrl false_branch ]
      | Ctrl Region -> fun { ctrl_inputs } -> List.map ctrl_inputs ~f:any_of_ctrl
      | Ctrl Loop -> fun { entry; backedge } -> [ any_of_ctrl entry; any_of_ctrl backedge ]
      | Ctrl (Function _) -> fun { call_sites } -> List.map call_sites ~f:any_of_ctrl
      | Ctrl Return -> fun { mem; data } -> [ any_of_mem mem; any_of_data data ]
      | Ctrl FunctionCall ->
          fun { fun_ptr; mem; args } ->
            any_of_data fun_ptr :: any_of_mem mem :: List.map args ~f:any_of_data
      | Ctrl FunctionCallEnd -> fun { ret_nodes } -> List.map ret_nodes ~f:any_of_ctrl
      | Mem New -> fun { mem; size } -> [ any_of_mem mem; any_of_data size ]
      | Mem (Load _) -> fun { mem; ptr } -> [ any_of_mem mem; any_of_mem ptr ]
      | Mem (Store _) ->
          fun { mem; ptr; value } -> [ any_of_mem mem; any_of_mem ptr; any_of_data value ]
      | Mem AddrOf -> fun { place; offset } -> [ any_of_data place; any_of_data offset ]
      | Mem (AddrOfField _) -> fun { place; offset } -> [ any_of_data place; any_of_data offset ]
      | Mem Deref -> fun { mem; ptr } -> [ any_of_mem mem; any_of_mem ptr ]
      | Mem Copy -> fun { mem; src; dst } -> [ any_of_mem mem; any_of_mem src; any_of_mem dst ]
      | Mem Phi -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_mem
      | Mem Param -> fun { phi_inputs } -> List.map phi_inputs ~f:any_of_mem
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
          | [ x ] -> { inp = data_of_any x }
          | _ -> assert false)
      | Data Phi -> fun lst -> { phi_inputs = List.map lst ~f:data_of_any }
      | Data (Param _) -> fun lst -> { phi_inputs = List.map lst ~f:data_of_any }
      | Data (External _) -> Fun.const ()
      | Data Cast -> (
          function
          | [ x ] -> { inp = data_of_any x }
          | _ -> assert false)
      | Ctrl Start -> Fun.const ()
      | Ctrl Stop -> (
          function
          | [ x ] -> { mem = mem_of_any x }
          | _ -> assert false)
      | Ctrl (Proj _) -> (
          function
          | [ x ] -> { inp = ctrl_of_any x }
          | _ -> assert false)
      | Ctrl If -> (
          function
          | [ x; y; z ] ->
              { cond = data_of_any x; true_branch = ctrl_of_any y; false_branch = ctrl_of_any z }
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
      | Mem (Load _) -> (
          function
          | [ x; y ] -> { mem = mem_of_any x; ptr = mem_of_any y }
          | _ -> assert false)
      | Mem (Store _) -> (
          function
          | [ x; y; z ] -> { mem = mem_of_any x; ptr = mem_of_any y; value = data_of_any z }
          | _ -> assert false)
      | Mem AddrOf -> (
          function
          | [ x; y ] -> { place = data_of_any x; offset = data_of_any y }
          | _ -> assert false)
      | Mem (AddrOfField _) -> (
          function
          | [ x; y ] -> { place = data_of_any x; offset = data_of_any y }
          | _ -> assert false)
      | Mem Deref -> (
          function
          | [ x; y ] -> { mem = mem_of_any x; ptr = mem_of_any y }
          | _ -> assert false)
      | Mem Copy -> (
          function
          | [ x; y; z ] -> { mem = mem_of_any x; src = mem_of_any y; dst = mem_of_any z }
          | _ -> assert false)
      | Mem Phi -> fun lst -> { phi_inputs = List.map lst ~f:mem_of_any }
      | Mem Param -> fun lst -> { phi_inputs = List.map lst ~f:mem_of_any }
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
  let pp _ _ = failwithf "todo %s" __LOC__ ()
  let show _ = failwithf "todo %s" __LOC__ ()

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
      | Mem AddrOf, Mem AddrOf -> Some (Type.Equal, Type.Equal)
      | Mem Deref, Mem Deref -> Some (Type.Equal, Type.Equal)
      | Mem Copy, Mem Copy -> Some (Type.Equal, Type.Equal)
      | Mem (Load _), Mem (Load _) -> Some (Type.Equal, Type.Equal)
      | Mem (Store _), Mem (Store _) -> Some (Type.Equal, Type.Equal)
      | Mem (AddrOfField _), Mem (AddrOfField _) -> Some (Type.Equal, Type.Equal)
      (* Misc *)
      | Scope _, Scope _ -> Some (Type.Equal, Type.Equal)
      | ForwardRef _, ForwardRef _ -> Some (Type.Equal, Type.Equal)
      | _, _ -> None

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
      | _ -> failwith "Not a control node"

  let[@inline] as_data_exn : type a b. (a, b) t -> (a, data) t =
     fun n ->
      match n.kind with
      | Data _ -> n
      | _ -> failwith "Not a data node"

  let[@inline] as_mem_exn : type a b. (a, b) t -> (a, mem) t =
     fun n ->
      match n.kind with
      | Mem _ -> n
      | _ -> failwith "Not a memory node"
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
