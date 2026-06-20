open Core

let id_counter = ref 0

let next_id () =
    incr id_counter;
    !id_counter

let reset_id () = id_counter := 0

module Z = struct
  include Z

  let sexp_of_t z = Sexp.Atom (Z.to_string z)

  let t_of_sexp = function
      | Sexp.Atom s -> Z.of_string s
      | _ -> failwith "Z.t_of_sexp: expected atom"

  let pp fmt z = Format.pp_print_string fmt (Z.to_string z)
end

module N = struct
  type cmp =
      | Eq
      | NEq
      | Lt
      | LEq
      | Gt
      | GEq
  [@@deriving show { with_path = false }, sexp_of]

  type any = AnyNode : ('a, 'tag) t -> any

  and _ ideal =
      | Loop : loop ideal
      | CProj : int -> unary ideal
      | MProj : int -> unary ideal
      | Start : unit ideal
      | Stop : stop ideal
      | Region : merge_point ideal
      | Phi : phi ideal
      | External : string -> extern_fun ideal

  and _ kind =
      | Int : Z.t -> unit kind
      | Ptr : unit kind
      | AddrOf : unary kind
      | Deref : deref kind
      | ZeroExtend : unary kind
      | SignExtend : unary kind
      | Add : binop kind
      | AddImm : Z.t -> unary kind
      | Sub : binop kind
      | SubImm : Z.t -> unary kind
      | Mul : binop kind
      | MulImm : Z.t -> unary kind
      | Div : binop kind
      | Lsh : binop kind
      | Rsh : binop kind
      | And : binop kind
      | Or : binop kind
      | LshImm : Z.t -> unary kind
      | RshImm : Z.t -> unary kind
      | AndImm : Z.t -> unary kind
      | OrImm : Z.t -> unary kind
      | Cmp : binop kind
      | CmpImm : Z.t -> unary kind
      | Set : cmp -> unary kind
      | JmpAlways : unit kind
      | Jmp : cmp -> unary kind
      | Mov : unary kind
      | DProj : int -> unary kind
      | FunctionProlog : int -> merge_point kind
      | Return : return kind
      | FunctionCall : int option -> fun_call kind
      | FunctionCallEnd : fun_call_end kind
      | Param : int -> phi kind
      | CalleeSave : Registers.reg -> unit kind
      | New : alloc kind
      | Store : store kind
      | Load : load kind
      | Noop : unit kind
      | RepMov : int -> repmov kind
      | Ideal : 'a ideal -> 'a kind

  and stop = { mem : any }
  and merge_point = { ctrl_inputs : any option list }

  and loop = {
      entry : any;
      backedge : any;
    }

  and phi = { phi_inputs : any option list }
  and unary = { input : any }

  and binop = {
      lhs : any;
      rhs : any;
    }

  and deref = {
      mem : any;
      ptr : any;
    }

  and alloc = {
      mem : any;
      size : any;
    }

  and store = {
      mem : any;
      ptr : any;
      value : any;
    }

  and load = {
      mem : any;
      ptr : any;
    }

  and repmov = {
      mem : any;
      src : any;
      dst : any;
    }

  and extern_fun = { params : any list }

  and fun_call = {
      fun_ptr : any option;
      mem : any;
      args : any list;
    }

  and fun_call_end = { ret_nodes : any list }

  and return = {
      mem : any;
      value : any;
      callee_saves : any list;
    }

  and ('a, 'b) t = {
      id : int;
      mutable kind : 'a kind;
      ir_node : Node.any;
      list_of_inputs : 'a -> any option list;
      inputs_of_list : any option list -> 'a;
      (* We only need this because the Graph.NODE interface needs two
         polymorphic parameters for the type t but the tag is otherwise unused.
         _tag_witness allows proving the tag is unit in the type_eq and unpack
         functions *)
      _tag_witness : (('b, unit) Type.eq[@sexp.opaque]);
    }
  [@@deriving sexp_of]

  let id n = n.id
  let list_of_inputs n inps = n.list_of_inputs inps
  let inputs_of_list n l = n.inputs_of_list l
  let equal a b = a.id = b.id
  let compare a b = Int.compare a.id b.id
  let hash a = Int.hash a.id

  let equal_kind : type a b. a kind -> b kind -> bool =
     fun a b ->
      let op_equal op op' =
          match (op, op') with
          | Eq, Eq -> true
          | NEq, NEq -> true
          | Lt, Lt -> true
          | LEq, LEq -> true
          | Gt, Gt -> true
          | GEq, GEq -> true
          | _, _ -> false
      in
      match (a, b) with
      | Int i, Int i' -> Z.equal i i'
      | Ptr, Ptr -> true
      | AddrOf, AddrOf -> true
      | Deref, Deref -> true
      | ZeroExtend, ZeroExtend -> true
      | SignExtend, SignExtend -> true
      | Add, Add -> true
      | AddImm i, AddImm i' -> Z.equal i i'
      | Sub, Sub -> true
      | SubImm i, SubImm i' -> Z.equal i i'
      | Mul, Mul -> true
      | MulImm i, MulImm i' -> Z.equal i i'
      | Div, Div -> true
      | Lsh, Lsh -> true
      | Rsh, Rsh -> true
      | And, And -> true
      | Or, Or -> true
      | LshImm i, LshImm i' -> Z.equal i i'
      | RshImm i, RshImm i' -> Z.equal i i'
      | AndImm i, AndImm i' -> Z.equal i i'
      | OrImm i, OrImm i' -> Z.equal i i'
      | Cmp, Cmp -> true
      | CmpImm i, CmpImm i' -> Z.equal i i'
      | Set op, Set op' -> op_equal op op'
      | JmpAlways, JmpAlways -> true
      | Jmp op, Jmp op' -> op_equal op op'
      | Mov, Mov -> true
      | DProj i, DProj i' -> i = i'
      | FunctionProlog i, FunctionProlog i' -> i = i'
      | Return, Return -> true
      | FunctionCall i, FunctionCall i' -> Option.equal Int.equal i i'
      | FunctionCallEnd, FunctionCallEnd -> true
      | Param i, Param i' -> i = i'
      | CalleeSave r, CalleeSave r' -> Registers.equal_reg r r'
      | New, New -> true
      | Store, Store -> true
      | Load, Load -> true
      | Noop, Noop -> true
      | RepMov i, RepMov i' -> i = i'
      | Ideal Loop, Ideal Loop -> true
      | Ideal (CProj i), Ideal (CProj i') -> i = i'
      | Ideal (MProj i), Ideal (MProj i') -> i = i'
      | Ideal Start, Ideal Start -> true
      | Ideal Stop, Ideal Stop -> true
      | Ideal Region, Ideal Region -> true
      | Ideal Phi, Ideal Phi -> true
      | Ideal (External s), Ideal (External s') -> String.equal s s'
      | Int i, _ -> false
      | Ptr, _ -> false
      | AddrOf, _ -> false
      | Deref, _ -> false
      | ZeroExtend, _ -> false
      | SignExtend, _ -> false
      | Add, _ -> false
      | AddImm i, _ -> false
      | Sub, _ -> false
      | SubImm i, _ -> false
      | Mul, _ -> false
      | MulImm i, _ -> false
      | Div, _ -> false
      | Lsh, _ -> false
      | Rsh, _ -> false
      | And, _ -> false
      | Or, _ -> false
      | LshImm i, _ -> false
      | RshImm i, _ -> false
      | AndImm i, _ -> false
      | OrImm i, _ -> false
      | Cmp, _ -> false
      | CmpImm i, _ -> false
      | Set op, _ -> false
      | JmpAlways, _ -> false
      | Jmp op, _ -> false
      | Mov, _ -> false
      | DProj i, _ -> false
      | FunctionProlog i, _ -> false
      | Return, _ -> false
      | FunctionCall i, _ -> false
      | FunctionCallEnd, _ -> false
      | Param i, _ -> false
      | CalleeSave r, _ -> false
      | New, _ -> false
      | Store, _ -> false
      | Load, _ -> false
      | Noop, _ -> false
      | RepMov i, _ -> false
      | Ideal Loop, _ -> false
      | Ideal (CProj i), _ -> false
      | Ideal (MProj i), _ -> false
      | Ideal Start, _ -> false
      | Ideal Stop, _ -> false
      | Ideal Region, _ -> false
      | Ideal Phi, _ -> false
      | Ideal (External s), _ -> false

  let rec pp_any fmt (AnyNode n) = pp fmt n

  and pp : type a b. Format.formatter -> (a, b) t -> unit =
     fun fmt n -> Format.fprintf fmt "(node #%d %a)" n.id pp_kind n.kind

  and pp_kind : type a. Format.formatter -> a kind -> unit =
     fun fmt k ->
      match k with
      | Int z -> Format.fprintf fmt "Int %s" (Z.to_string z)
      | Ptr -> Format.pp_print_string fmt "Ptr"
      | Add -> Format.pp_print_string fmt "Add"
      | Sub -> Format.pp_print_string fmt "Sub"
      | Mul -> Format.pp_print_string fmt "Mul"
      | Div -> Format.pp_print_string fmt "Div"
      | Lsh -> Format.pp_print_string fmt "Lsh"
      | Rsh -> Format.pp_print_string fmt "Rsh"
      | And -> Format.pp_print_string fmt "And"
      | Or -> Format.pp_print_string fmt "Or"
      | Cmp -> Format.pp_print_string fmt "Cmp"
      | JmpAlways -> Format.pp_print_string fmt "JmpAlways"
      | AddrOf -> Format.pp_print_string fmt "AddrOf"
      | Deref -> Format.pp_print_string fmt "Deref"
      | ZeroExtend -> Format.pp_print_string fmt "ZeroExtend"
      | SignExtend -> Format.pp_print_string fmt "SignExtend"
      | Mov -> Format.pp_print_string fmt "Mov"
      | Return -> Format.pp_print_string fmt "Return"
      | Noop -> Format.pp_print_string fmt "Noop"
      | FunctionCallEnd -> Format.pp_print_string fmt "FunctionCallEnd"
      | AddImm z -> Format.fprintf fmt "AddImm %s" (Z.to_string z)
      | SubImm z -> Format.fprintf fmt "SubImm %s" (Z.to_string z)
      | MulImm z -> Format.fprintf fmt "MulImm %s" (Z.to_string z)
      | LshImm z -> Format.fprintf fmt "LshImm %s" (Z.to_string z)
      | RshImm z -> Format.fprintf fmt "RshImm %s" (Z.to_string z)
      | AndImm z -> Format.fprintf fmt "AndImm %s" (Z.to_string z)
      | OrImm z -> Format.fprintf fmt "OrImm %s" (Z.to_string z)
      | CmpImm z -> Format.fprintf fmt "CmpImm %s" (Z.to_string z)
      | Set cmp -> Format.fprintf fmt "Set %a" pp_cmp cmp
      | Jmp cmp -> Format.fprintf fmt "Jmp %a" pp_cmp cmp
      | DProj i -> Format.fprintf fmt "DProj %d" i
      | FunctionProlog i -> Format.fprintf fmt "FunctionProlog %d" i
      | FunctionCall None -> Format.pp_print_string fmt "FunctionCall indirect"
      | FunctionCall (Some i) -> Format.fprintf fmt "FunctionCall %d" i
      | Param i -> Format.fprintf fmt "Param %d" i
      | RepMov i -> Format.fprintf fmt "RepMov %d" i
      | CalleeSave r -> Format.fprintf fmt "CalleeSave %a" Registers.pp_reg r
      | New -> Format.pp_print_string fmt "New"
      | Store -> Format.pp_print_string fmt "Store"
      | Load -> Format.pp_print_string fmt "Load"
      | Ideal i -> Format.fprintf fmt "Ideal(%a)" pp_ideal i

  and pp_ideal : type a. Format.formatter -> a ideal -> unit =
     fun fmt i ->
      match i with
      | Loop -> Format.pp_print_string fmt "Loop"
      | CProj n -> Format.fprintf fmt "CProj %d" n
      | MProj n -> Format.fprintf fmt "MProj %d" n
      | Start -> Format.pp_print_string fmt "Start"
      | Stop -> Format.pp_print_string fmt "Stop"
      | Region -> Format.pp_print_string fmt "Region"
      | Phi -> Format.pp_print_string fmt "Phi"
      | External s -> Format.fprintf fmt "External %s" s

  let show : type a b. (a, b) t -> string = fun n -> Format.asprintf "%a" pp n
  let show_any = Format.asprintf "%a" pp_any
  let show_kind : type a. a kind -> string = fun k -> Format.asprintf "%a" pp_kind k

  let get_list_of_inputs : type a. a kind -> a -> any option list =
     fun k ->
      let binop_inputs = fun { lhs; rhs } -> [ Some lhs; Some rhs ] in
      let unary_inputs = fun { input } -> [ Some input ] in
      match k with
      | Int _ -> Fun.const []
      | Ptr -> Fun.const []
      | AddrOf -> unary_inputs
      | Deref -> fun { mem; ptr } -> [ Some mem; Some ptr ]
      | ZeroExtend -> unary_inputs
      | SignExtend -> unary_inputs
      | Add -> binop_inputs
      | AddImm _ -> unary_inputs
      | Sub -> binop_inputs
      | SubImm _ -> unary_inputs
      | Mul -> binop_inputs
      | MulImm _ -> unary_inputs
      | Div -> binop_inputs
      | Lsh -> binop_inputs
      | Rsh -> binop_inputs
      | And -> binop_inputs
      | Or -> binop_inputs
      | LshImm _ -> unary_inputs
      | RshImm _ -> unary_inputs
      | AndImm _ -> unary_inputs
      | OrImm _ -> unary_inputs
      | Cmp -> binop_inputs
      | CmpImm _ -> unary_inputs
      | Set _ -> unary_inputs
      | JmpAlways -> Fun.const []
      | Jmp _ -> unary_inputs
      | Mov -> unary_inputs
      | DProj _ -> unary_inputs
      | FunctionProlog _ -> fun { ctrl_inputs } -> ctrl_inputs
      | Return ->
          fun { mem; value; callee_saves } ->
            [ Some mem; Some value ] @ List.map callee_saves ~f:Option.some
      | FunctionCall _ ->
          fun { fun_ptr; mem; args } -> [ fun_ptr; Some mem ] @ List.map args ~f:Option.some
      | FunctionCallEnd -> fun { ret_nodes } -> List.map ret_nodes ~f:Option.some
      | Param _ -> fun { phi_inputs } -> phi_inputs
      | CalleeSave _ -> Fun.const []
      | New -> fun { mem; size } -> [ Some mem; Some size ]
      | Store -> fun { mem; ptr; value } -> [ Some mem; Some ptr; Some value ]
      | Load -> fun { mem; ptr } -> [ Some mem; Some ptr ]
      | Noop -> Fun.const []
      | RepMov _ -> fun { mem; src; dst } -> [ Some mem; Some src; Some dst ]
      | Ideal Loop -> fun { entry; backedge } -> [ Some entry; Some backedge ]
      | Ideal (CProj _) -> fun { input } -> [ Some input ]
      | Ideal (MProj _) -> fun { input } -> [ Some input ]
      | Ideal Start -> Fun.const []
      | Ideal Stop -> fun { mem } -> [ Some mem ]
      | Ideal Region -> fun { ctrl_inputs } -> ctrl_inputs
      | Ideal Phi -> fun { phi_inputs } -> phi_inputs
      | Ideal (External _) -> fun { params } -> List.map params ~f:Option.some

  let get_inputs_of_list : type a. a kind -> any option list -> a =
     fun k ->
      let unary_inputs = function
          | [ x ] -> { input = Option.value_exn x }
          | _ -> assert false
      in
      let binop_inputs = function
          | [ x; y ] -> { lhs = Option.value_exn x; rhs = Option.value_exn y }
          | _ -> assert false
      in
      match k with
      | Int _ -> Fun.const ()
      | Ptr -> Fun.const ()
      | AddrOf -> unary_inputs
      | Deref -> (
          function
          | [ x; y ] -> { mem = Option.value_exn x; ptr = Option.value_exn y }
          | _ -> assert false)
      | ZeroExtend -> unary_inputs
      | SignExtend -> unary_inputs
      | Add -> binop_inputs
      | AddImm _ -> unary_inputs
      | Sub -> binop_inputs
      | SubImm _ -> unary_inputs
      | Mul -> binop_inputs
      | MulImm _ -> unary_inputs
      | Div -> binop_inputs
      | Lsh -> binop_inputs
      | Rsh -> binop_inputs
      | And -> binop_inputs
      | Or -> binop_inputs
      | LshImm _ -> unary_inputs
      | RshImm _ -> unary_inputs
      | AndImm _ -> unary_inputs
      | OrImm _ -> unary_inputs
      | Cmp -> binop_inputs
      | CmpImm _ -> unary_inputs
      | Set _ -> unary_inputs
      | JmpAlways -> Fun.const ()
      | Jmp _ -> unary_inputs
      | Mov -> unary_inputs
      | DProj _ -> unary_inputs
      | FunctionProlog _ -> fun lst -> { ctrl_inputs = lst }
      | Return -> (
          function
          | x :: y :: rest ->
              {
                mem = Option.value_exn x;
                value = Option.value_exn y;
                callee_saves = List.map rest ~f:(fun o -> Option.value_exn o);
              }
          | _ -> assert false)
      | FunctionCall _ -> (
          function
          | fun_ptr :: mem :: rest ->
              {
                fun_ptr;
                mem = Option.value_exn mem;
                args = List.map rest ~f:(fun o -> Option.value_exn o);
              }
          | _ -> assert false)
      | FunctionCallEnd -> fun lst -> { ret_nodes = List.map lst ~f:(fun o -> Option.value_exn o) }
      | Param _ -> fun lst -> { phi_inputs = lst }
      | CalleeSave _ -> Fun.const ()
      | New -> (
          function
          | [ x; y ] -> { mem = Option.value_exn x; size = Option.value_exn y }
          | _ -> assert false)
      | Store -> (
          function
          | [ x; y; z ] ->
              { mem = Option.value_exn x; ptr = Option.value_exn y; value = Option.value_exn z }
          | _ -> assert false)
      | Load -> (
          function
          | [ x; y ] -> { mem = Option.value_exn x; ptr = Option.value_exn y }
          | _ -> assert false)
      | Noop -> Fun.const ()
      | RepMov _ -> (
          function
          | [ x; y; z ] ->
              { mem = Option.value_exn x; src = Option.value_exn y; dst = Option.value_exn z }
          | _ -> assert false)
      | Ideal Loop -> (
          function
          | [ x; y ] -> { entry = Option.value_exn x; backedge = Option.value_exn y }
          | _ -> assert false)
      | Ideal (CProj _) -> unary_inputs
      | Ideal (MProj _) -> unary_inputs
      | Ideal Start -> Fun.const ()
      | Ideal Stop -> (
          function
          | [ x ] -> { mem = Option.value_exn x }
          | _ -> assert false)
      | Ideal Region -> fun lst -> { ctrl_inputs = lst }
      | Ideal Phi -> fun lst -> { phi_inputs = lst }
      | Ideal (External _) -> fun lst -> { params = List.map lst ~f:(fun o -> Option.value_exn o) }

  let[@inline] kind_eq : type a b. a kind -> b kind -> (a, b) Type.eq option =
     fun a b ->
      match (a, b) with
      | Int _, Int _ -> Some Type.Equal
      | Ptr, Ptr -> Some Type.Equal
      | AddrOf, AddrOf -> Some Type.Equal
      | Deref, Deref -> Some Type.Equal
      | ZeroExtend, ZeroExtend -> Some Type.Equal
      | SignExtend, SignExtend -> Some Type.Equal
      | Add, Add -> Some Type.Equal
      | AddImm _, AddImm _ -> Some Type.Equal
      | Sub, Sub -> Some Type.Equal
      | SubImm _, SubImm _ -> Some Type.Equal
      | Mul, Mul -> Some Type.Equal
      | MulImm _, MulImm _ -> Some Type.Equal
      | Div, Div -> Some Type.Equal
      | Lsh, Lsh -> Some Type.Equal
      | Rsh, Rsh -> Some Type.Equal
      | And, And -> Some Type.Equal
      | Or, Or -> Some Type.Equal
      | LshImm _, LshImm _ -> Some Type.Equal
      | RshImm _, RshImm _ -> Some Type.Equal
      | AndImm _, AndImm _ -> Some Type.Equal
      | OrImm _, OrImm _ -> Some Type.Equal
      | Cmp, Cmp -> Some Type.Equal
      | CmpImm _, CmpImm _ -> Some Type.Equal
      | Set _, Set _ -> Some Type.Equal
      | JmpAlways, JmpAlways -> Some Type.Equal
      | Jmp _, Jmp _ -> Some Type.Equal
      | Mov, Mov -> Some Type.Equal
      | DProj _, DProj _ -> Some Type.Equal
      | FunctionProlog _, FunctionProlog _ -> Some Type.Equal
      | Return, Return -> Some Type.Equal
      | FunctionCall _, FunctionCall _ -> Some Type.Equal
      | FunctionCallEnd, FunctionCallEnd -> Some Type.Equal
      | Param _, Param _ -> Some Type.Equal
      | CalleeSave _, CalleeSave _ -> Some Type.Equal
      | New, New -> Some Type.Equal
      | Store, Store -> Some Type.Equal
      | Load, Load -> Some Type.Equal
      | Noop, Noop -> Some Type.Equal
      | RepMov _, RepMov _ -> Some Type.Equal
      | Ideal Loop, Ideal Loop -> Some Type.Equal
      | Ideal (CProj _), Ideal (CProj _) -> Some Type.Equal
      | Ideal (MProj _), Ideal (MProj _) -> Some Type.Equal
      | Ideal Start, Ideal Start -> Some Type.Equal
      | Ideal Stop, Ideal Stop -> Some Type.Equal
      | Ideal Region, Ideal Region -> Some Type.Equal
      | Ideal Phi, Ideal Phi -> Some Type.Equal
      | Ideal (External _), Ideal (External _) -> Some Type.Equal
      | Int _, _ -> None
      | Ptr, _ -> None
      | AddrOf, _ -> None
      | Deref, _ -> None
      | ZeroExtend, _ -> None
      | SignExtend, _ -> None
      | Add, _ -> None
      | AddImm _, _ -> None
      | Sub, _ -> None
      | SubImm _, _ -> None
      | Mul, _ -> None
      | MulImm _, _ -> None
      | Div, _ -> None
      | Lsh, _ -> None
      | Rsh, _ -> None
      | And, _ -> None
      | Or, _ -> None
      | LshImm _, _ -> None
      | RshImm _, _ -> None
      | AndImm _, _ -> None
      | OrImm _, _ -> None
      | Cmp, _ -> None
      | CmpImm _, _ -> None
      | Set _, _ -> None
      | JmpAlways, _ -> None
      | Jmp _, _ -> None
      | Mov, _ -> None
      | DProj _, _ -> None
      | FunctionProlog _, _ -> None
      | Return, _ -> None
      | FunctionCall _, _ -> None
      | FunctionCallEnd, _ -> None
      | Param _, _ -> None
      | CalleeSave _, _ -> None
      | New, _ -> None
      | Store, _ -> None
      | Load, _ -> None
      | Noop, _ -> None
      | RepMov _, _ -> None
      | Ideal Loop, _ -> None
      | Ideal (CProj _), _ -> None
      | Ideal (MProj _), _ -> None
      | Ideal Start, _ -> None
      | Ideal Stop, _ -> None
      | Ideal Region, _ -> None
      | Ideal Phi, _ -> None
      | Ideal (External _), _ -> None

  let[@inline] type_eq : type a b taga tagb.
      (a, taga) t -> (b, tagb) t -> ((a, b) Type.eq * (taga, tagb) Type.eq) option =
     fun a b ->
      match kind_eq a.kind b.kind with
      | None -> None
      | Some eq -> (
          match (a._tag_witness, b._tag_witness) with
          | Type.Equal, Type.Equal -> Some (eq, Type.Equal))

  let[@inline] unpack : type a b c. (a, c) t -> b kind -> (b, unit) t option =
     fun n k ->
      match kind_eq n.kind k with
      | Some Type.Equal -> (
          match n._tag_witness with
          | Type.Equal -> Some n)
      | None -> None

  let[@inline] unpack_exn : type a b c. (a, c) t -> b kind -> (b, unit) t =
     fun n k -> unpack n k |> Option.value_exn

  let[@inline] fix_tag : type a b. (a, b) t -> (a, unit) t =
     fun a ->
      match a._tag_witness with
      | Type.Equal -> a
end

include N
module G = Graph.Make (N)

let invert_cond cond =
    match cond with
    | Eq -> NEq
    | NEq -> Eq
    | Lt -> GEq
    | LEq -> Gt
    | Gt -> LEq
    | GEq -> Lt

let is_cheap_to_clone : type a b. (a, b) t -> bool =
   fun n ->
    match n.kind with
    | Int _ -> true
    | Ptr -> true
    | Cmp -> true
    | CmpImm _ -> true
    | _ -> false

let is_control_node : type a b. (a, b) t -> bool =
   fun n ->
    match n.kind with
    | Jmp _
    | JmpAlways
    | FunctionProlog _
    | FunctionCall _
    | FunctionCallEnd
    | Return
    | Ideal Start
    | Ideal Stop
    | Ideal Loop
    | Ideal Region
    | Ideal (CProj _) ->
        true
    | Ideal Phi
    | Ideal (MProj _)
    | Add
    | AddImm _
    | Sub
    | SubImm _
    | Mul
    | MulImm _
    | Div
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm _
    | RshImm _
    | AndImm _
    | OrImm _
    | Cmp
    | CmpImm _
    | Set _
    | Int _
    | Ptr
    | Mov
    | DProj _
    | Param _
    | New
    | Store
    | Load
    | CalleeSave _
    | Ideal (External _)
    | Noop
    | ZeroExtend
    | SignExtend
    | AddrOf
    | Deref
    | RepMov _ ->
        false

let is_blockhead : type a b. (a, b) t -> bool =
   fun n ->
    match n.kind with
    | Ideal Start
    | Ideal (CProj _)
    | Ideal Region
    | Ideal Loop
    | Ideal Stop
    | FunctionProlog _
    | FunctionCallEnd ->
        true
    | _ -> false

let is_two_address : type a b. (a, b) t -> bool =
   fun n ->
    match n.kind with
    | Add
    | AddImm _
    | Sub
    | SubImm _
    | Mul
    | MulImm _
    | Lsh
    | LshImm _
    | Rsh
    | RshImm _
    | And
    | AndImm _
    | Or
    | OrImm _ ->
        true
    | Int _
    | Ptr
    | AddrOf
    | Deref
    | Div
    | Cmp
    | CmpImm _
    | Set _
    | JmpAlways
    | Jmp _
    | Mov
    | ZeroExtend
    | SignExtend
    | DProj _
    | FunctionProlog _
    | Return
    | FunctionCall _
    | FunctionCallEnd
    | Param _
    | CalleeSave _
    | New
    | Store
    | Load
    | Ideal _
    | Noop
    | RepMov _ ->
        false

let is_multi_output n =
    let (AnyNode ir_node) = n.ir_node in
    match ir_node.typ with
    | Tuple _ -> true
    | _ -> false

let get_in_reg_mask : type a b. G.readonly G.t -> (a, b) t -> int -> Registers.Mask.t option =
   fun _ n i ->
    match n.kind with
    | Add
    | Sub
    | Mul ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.general_r)
    | AddImm _
    | SubImm _
    | MulImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Div ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.rax else Registers.Mask.div)
    | Lsh
    | Rsh ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.cl)
    | And
    | Or ->
        assert (i <= 1);
        Some (if i = 0 then Registers.Mask.general_w else Registers.Mask.general_r)
    | LshImm _
    | RshImm _
    | AndImm _
    | OrImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Int _ -> None
    | Ptr -> None
    | DProj _ -> None
    | Cmp ->
        assert (i <= 1);
        Some Registers.Mask.general_r
    | CmpImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_r
    | Set _ ->
        assert (i = 0);
        Some Registers.Mask.flags
    | JmpAlways -> None
    | Jmp _ ->
        assert (i = 0);
        Some Registers.Mask.flags
    | Mov ->
        assert (i = 0);
        Some Registers.Mask.all_and_stack
    | FunctionProlog _ -> None
    | Return ->
        (* inputs: 0: memory; 1: value; 2+: callee saved regs *)
        let (AnyNode ir_node) = n.ir_node in
        let is_void =
            match ir_node.typ with
            | Tuple (Value [ _; _; t ]) -> Types.equal t Void
            | _ -> false
        in
        if i = 0 then
          None
        else if i = 1 then
          if is_void then
            None
          else
            Some Registers.Mask.rax (* retutrn value *)
        else
          let calle_saved = Registers.Mask.callee_save |> Registers.Mask.to_list in
          Some (Registers.Mask.of_list [ List.nth_exn calle_saved (i - 2) ])
    | FunctionCall (Some _) ->
        (* skip non existant fun ptr input and the memory input, known function call so no fun_ptr as input which makes memory at index 0 *)
        if i = 0 || i = 1 then
          None
        else
          Some (Registers.Mask.x64_systemv (i - 2))
    | FunctionCall None ->
        (* When the call target is not compile time known the first input to
           the FunctionCall node is the function ptr then memory and arguments come after *)
        if i = 0 then
          Some Registers.Mask.general_w
        else if i = 1 then
          None
        else
          Some (Registers.Mask.x64_systemv (i - 2))
    | FunctionCallEnd -> None
    | Param _ -> None
    | CalleeSave _ -> None
    | New -> if i = 1 then Some Registers.Mask.general_r else None
    | Store -> (
        match i with
        | 0 -> None
        | 1 -> Some Registers.Mask.general_r (* base*)
        | 2 -> Some Registers.Mask.general_r (* index *)
        | 3 -> Some Registers.Mask.general_r (* value *)
        | _ -> failwithf "Invalid index %d for input reg mask of %s" i (show n) ())
    | Load -> (
        match i with
        | 0 -> None
        | 1 -> Some Registers.Mask.general_r (* base*)
        | 2 -> Some Registers.Mask.general_r (* index *)
        | _ -> failwithf "Invalid index %d for input reg mask of %s" i (show n) ())
    | Noop -> None
    | ZeroExtend ->
        assert (i = 0);
        Some Registers.Mask.general_r
    | SignExtend ->
        assert (i = 0);
        Some Registers.Mask.general_r
    | AddrOf ->
        assert (i = 0);
        Some Registers.Mask.all_and_stack
    | Deref ->
        assert (i = 1);
        Some Registers.Mask.all_and_stack
    | RepMov _ ->
        assert (i <= 2);
        if i = 1 then
          Some (Registers.Mask.of_list [ Reg RSI ])
        else if i = 2 then
          Some (Registers.Mask.of_list [ Reg RDI ])
        else
          None
    | Ideal _ -> None

let rec get_out_reg_mask : type a b. G.readonly G.t -> (a, b) t -> int -> Registers.Mask.t option =
   fun g n i ->
    match n.kind with
    | Add
    | Sub
    | Mul
    | AddImm _
    | SubImm _
    | MulImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Div ->
        assert (i = 0);
        Some Registers.Mask.rax
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm _
    | RshImm _
    | AndImm _
    | OrImm _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Int _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Ptr ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | DProj proj_i ->
        let { input = AnyNode in_node } = G.get_dependencies_exn g n in
        get_out_reg_mask g in_node proj_i
    | Cmp
    | CmpImm _ ->
        assert (i = 0);
        Some Registers.Mask.flags
    | Set _ ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Mov ->
        assert (i = 0);
        Some Registers.Mask.spill
    | JmpAlways -> None
    | Jmp _ -> None
    | FunctionProlog _ -> None
    | FunctionCall _ -> None
    | FunctionCallEnd ->
        let (AnyNode ir_node) = n.ir_node in
        let is_void =
            match ir_node.typ with
            | Tuple (Value [ _; _; t ]) -> Types.equal t Void
            | _ -> false
        in
        if (not is_void) && i = 2 then
          Some Registers.Mask.rax
        else
          None
    | Param idx ->
        assert (i = 0);
        (* skip memory param *)
        if idx < 0 then
          None
        else
          Some (Registers.Mask.x64_systemv idx)
    | CalleeSave reg -> Some (Registers.Mask.of_list [ Reg reg ])
    | Return ->
        assert (i = 0);
        let (AnyNode ir_node) = n.ir_node in
        let is_void =
            match ir_node.typ with
            | Tuple (Value [ _; _; t ]) -> Types.equal t Void
            | _ -> false
        in
        if i = 0 && not is_void then Some Registers.Mask.rax else None
    | New -> if i = 1 then Some Registers.Mask.general_w else None
    | Store -> None
    | Load -> Some Registers.Mask.general_w
    | Noop -> None
    | ZeroExtend ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | SignExtend ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | AddrOf ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | Deref ->
        assert (i = 0);
        Some Registers.Mask.general_w
    | RepMov _ -> None
    | Ideal _ -> None

let get_register_kills : type a b. (a, b) t -> Registers.Mask.t option =
   fun n ->
    match n.kind with
    | FunctionCall _ -> Some Registers.Mask.caller_save
    | Div -> Some (Registers.Mask.of_list [ Reg RDX ])
    | RepMov _ ->
        (* HACK: can't create the input nodes in of_mem_node since original
           graph is read only so we'll compute the correct  count
           inside asm emit. So we need to kill the reg of rep mov to
           make reg allocator work *)
        Some (Registers.Mask.of_list [ Reg RCX ])
    | _ -> None

let create_node : type a. a kind -> Node.any -> (a, unit) t =
   fun kind ir_node ->
    {
      id = next_id ();
      kind;
      ir_node;
      list_of_inputs = get_list_of_inputs kind;
      inputs_of_list = get_inputs_of_list kind;
      _tag_witness = Type.Equal;
    }

let get_phi_backedge g phi =
    let { phi_inputs } = G.get_dependencies_exn g phi in
    assert (List.length phi_inputs = 2);
    List.nth_exn phi_inputs 1

let rec of_data_node : type a.
    (Node.any, any) Hashtbl.t ->
    Node.G.readonly Node.G.t ->
    G.readwrite G.t ->
    (a, Node.data) Node.t ->
    any =
   fun memo g machine_g n ->
    let set_ctrl : type a b. (a, b) t -> unit =
       fun node ->
        let ctrl =
            Node.G.get_ctrl g n
            |> Option.map ~f:(fun (AnyNode ctrl) -> convert_node memo g machine_g ctrl)
        in
        match ctrl with
        | None -> ()
        | Some (AnyNode ctrl) -> G.set_ctrl machine_g node ctrl
    in
    let get_const (Node.AnyData n) =
        if Types.is_constant n.typ && Z.fits_int32 (Types.get_integer_const_exn n.typ) then
          Some (Types.get_integer_const_exn n.typ)
        else
          None
    in
    let binop_commutative kind kind_imm (n : (Node.binop, Node.data) Node.t) =
        let deps = Node.G.get_dependencies_exn g n in
        let lhs = Option.value_exn deps.lhs in
        let rhs = Option.value_exn deps.rhs in
        match (get_const lhs, get_const rhs) with
        | None, None ->
            let node = create_node kind (Node.AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData lhs) = lhs in
            let lhs = convert_node memo g machine_g lhs in
            let (AnyData rhs) = rhs in
            let rhs = convert_node memo g machine_g rhs in
            G.add_node machine_g node { lhs; rhs };
            set_ctrl node;
            AnyNode node
        | _, Some c ->
            let kind = kind_imm c in
            let node = create_node kind (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData normal) = lhs in
            let lhs = convert_node memo g machine_g normal in
            G.add_node machine_g node { input = lhs };
            set_ctrl node;
            AnyNode node
        | Some c, _ ->
            let kind = kind_imm c in
            let node = create_node kind (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData normal) = rhs in
            let lhs = convert_node memo g machine_g normal in
            G.add_node machine_g node { input = lhs };
            set_ctrl node;
            AnyNode node
    in
    let binop_non_commutative kind kind_imm (n : (Node.binop, Node.data) Node.t) =
        let deps = Node.G.get_dependencies_exn g n in
        let (AnyData rhs) = Option.value_exn deps.rhs in
        match rhs.typ with
        | Integer _
          when Types.is_constant rhs.typ && Z.fits_int32 (Types.get_integer_const_exn rhs.typ) ->
            let v = Types.get_integer_const_exn rhs.typ in
            let kind = kind_imm v in
            let node = create_node kind (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData lhs) = Option.value_exn deps.lhs in
            let lhs = convert_node memo g machine_g lhs in
            G.add_node machine_g node { input = lhs };
            set_ctrl node;
            AnyNode node
        | _ ->
            let node = create_node kind (Node.AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData lhs) = Option.value_exn deps.lhs in
            let lhs = convert_node memo g machine_g lhs in
            let rhs = convert_node memo g machine_g rhs in
            G.add_node machine_g node { lhs; rhs };
            set_ctrl node;
            AnyNode node
    in
    let simple : type a b t. a kind -> (b, t) Node.t -> (b -> a) -> any =
       fun kind n f ->
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let inputs = Node.G.get_dependencies_exn g n in
        G.add_node machine_g node (f inputs);
        set_ctrl node;
        AnyNode node
    in
    match n.kind with
    | ForwardRef _ -> failwith "ForwardRef nodes shouldn't exist by this point"
    | Data Add -> binop_commutative Add (fun i -> AddImm i) n
    | Data Sub -> binop_non_commutative Sub (fun i -> SubImm i) n
    | Data Mul -> binop_commutative Mul (fun i -> MulImm i) n
    | Data Div ->
        let kind = Div in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
        let (AnyData lhs) = Option.value_exn lhs in
        let (AnyData rhs) = Option.value_exn rhs in
        let lhs = convert_node memo g machine_g lhs in
        let rhs = convert_node memo g machine_g rhs in
        G.add_node machine_g node { lhs; rhs };
        set_ctrl node;
        AnyNode node
    | Data Constant ->
        let kind =
            match n.typ with
            | Integer _ when Types.is_constant n.typ -> Int (Types.get_integer_const_exn n.typ)
            | Ptr _ -> Ptr
            | FunPtr _ -> Ptr
            | Struct _ -> Ptr
            | Void -> Noop
            | Bool (Value b) -> Int (Bool.to_int b |> Z.of_int)
            | _ -> assert false
        in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        G.add_node machine_g node ();
        set_ctrl node;
        AnyNode node
    | Data (Proj i) ->
        let kind = DProj i in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.input } = Node.G.get_dependencies_exn g n in
        let (AnyNode input) = Option.value_exn input in
        let input = convert_node memo g machine_g input in
        G.add_node machine_g node { input };
        set_ctrl node;
        AnyNode node
    | Data Eq
    | Data NEq
    | Data Lt
    | Data LEq
    | Data Gt
    | Data GEq -> (
        let as_binop : type a b. (a, b) Node.t -> (Node.binop, Node.data) Node.t =
           fun n ->
            match n.kind with
            | Data Add -> n
            | Data Sub -> n
            | Data Div -> n
            | Data Mul -> n
            | Data Eq -> n
            | Data NEq -> n
            | Data Lt -> n
            | Data LEq -> n
            | Data Gt -> n
            | Data GEq -> n
            | Data BAnd -> n
            | Data BOr -> n
            | _ -> assert false
        in
        let n = as_binop n in
        let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
        let m_cmp =
            match n.kind with
            | Data Eq -> Eq
            | Data NEq -> NEq
            | Data Lt -> Lt
            | Data LEq -> LEq
            | Data Gt -> Gt
            | Data GEq -> GEq
            | _ -> assert false
        in

        let lhs = Option.value_exn lhs in
        let rhs = Option.value_exn rhs in
        match (get_const lhs, get_const rhs) with
        | None, None ->
            let set_node = create_node (Set m_cmp) (AnyNode n) in
            let cmp_node = create_node Cmp (AnyNode n) in

            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode set_node);
            let (AnyData lhs) = lhs in
            let lhs = convert_node memo g machine_g lhs in
            let (AnyData rhs) = rhs in
            let rhs = convert_node memo g machine_g rhs in
            G.add_node machine_g cmp_node { lhs; rhs };
            set_ctrl cmp_node;
            G.add_node machine_g set_node { input = AnyNode cmp_node };
            set_ctrl set_node;
            AnyNode set_node
        | _, Some c ->
            let set_node = create_node (Set m_cmp) (AnyNode n) in
            let cmp_node = create_node (CmpImm c) (AnyNode n) in

            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode set_node);
            let (AnyData lhs) = lhs in
            let lhs = convert_node memo g machine_g lhs in
            G.add_node machine_g cmp_node { input = lhs };
            set_ctrl cmp_node;
            G.add_node machine_g set_node { input = AnyNode cmp_node };
            set_ctrl set_node;
            AnyNode set_node
        | Some c, _ ->
            (* lhs is immediate so we have to invert the comparisn to put the immediate on right side *)
            let m_cmp =
                match m_cmp with
                | Eq
                | NEq ->
                    m_cmp
                | Lt -> Gt
                | LEq -> GEq
                | Gt -> Lt
                | GEq -> LEq
            in
            let set_node = create_node (Set m_cmp) (AnyNode n) in
            let cmp_node = create_node (CmpImm c) (AnyNode n) in

            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode set_node);
            let (AnyData rhs) = rhs in
            let rhs = convert_node memo g machine_g rhs in
            G.add_node machine_g cmp_node { input = rhs };
            set_ctrl cmp_node;
            G.add_node machine_g set_node { input = AnyNode cmp_node };
            set_ctrl set_node;
            AnyNode set_node)
    | Data Phi ->
        let kind = Ideal Phi in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node.AnyData n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node
    | Data Lsh -> binop_non_commutative Lsh (fun i -> LshImm i) n
    | Data Rsh -> binop_non_commutative Rsh (fun i -> RshImm i) n
    | Data BAnd -> binop_commutative And (fun i -> AndImm i) n
    | Data BOr -> binop_commutative Or (fun i -> OrImm i) n
    | Data (Param i) ->
        let kind = Param i in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node.AnyData n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node
    | Data (External name) ->
        let node = create_node (Ideal (External name)) (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.params } = Node.G.get_dependencies_exn g n in
        let params = List.map params ~f:(fun (AnyData n) -> convert_node memo g machine_g n) in
        G.add_node machine_g node { params };
        set_ctrl node;
        AnyNode node
    | Data Cast ->
        let { Node.input } = Node.G.get_dependencies_exn g n in
        let (AnyData input) = Option.value_exn input in
        let kind =
            match input.typ with
            | Integer (Value { min; max; num_widens; fixed_width }) ->
                let is_unsigned = Z.geq min Z.zero in
                if is_unsigned then
                  ZeroExtend
                else
                  SignExtend
            | _ -> failwithf "TODO: not handled yet %s" (Types.show input.typ) ()
        in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let input = convert_node memo g machine_g input in
        G.add_node machine_g node { input };
        set_ctrl node;
        AnyNode node
    | Data (Load _) -> (
        (* TODO check for ops like add that can address memory directly *)
        match n.typ with
        | Struct _ ->
            let node = create_node AddrOf (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let { Node.mem = _; ptr } : Node.load = Node.G.get_dependencies_exn g n in
            let (AnyData ptr) = Option.value_exn ptr in
            let ptr = convert_node memo g machine_g ptr in
            G.add_node machine_g node { input = ptr };
            set_ctrl node;
            AnyNode node
        | _ ->
            simple Load n (fun { Node.mem; ptr } ->
                let (AnyMem mem) = Option.value_exn mem in
                let (AnyData ptr) = Option.value_exn ptr in
                { mem = convert_node memo g machine_g mem; ptr = convert_node memo g machine_g ptr })
        )
    | Data AddrOf -> (
        let node = create_node AddrOf (AnyNode n) in
        let { Node.place; offset } = Node.G.get_dependencies_exn g n in
        match offset with
        | None ->
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData place) = Option.value_exn place in
            let place = convert_node memo g machine_g place in
            G.add_node machine_g node { input = place };
            set_ctrl node;
            AnyNode node
        | Some (AnyData offset) ->
            let add_node = create_node Add (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode add_node);
            let (AnyData place) = Option.value_exn place in
            let place = convert_node memo g machine_g place in
            let offset = convert_node memo g machine_g offset in
            G.add_node machine_g node { input = place };
            set_ctrl node;
            G.add_node machine_g add_node { lhs = AnyNode node; rhs = offset };
            set_ctrl add_node;
            AnyNode add_node)
    | Data (AddrOfField f) ->
        let { Node.place; offset } = Node.G.get_dependencies_exn g n in
        let (AnyData place) = Option.value_exn place in
        let field_offset = Types.get_offset place.typ f |> Option.value_exn in

        let base_addr = create_node AddrOf (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode base_addr);
        let place = convert_node memo g machine_g place in
        G.add_node machine_g base_addr { input = place };
        set_ctrl base_addr;

        let field_addr = create_node (AddImm (Z.of_int field_offset)) (AnyNode n) in
        G.add_node machine_g field_addr { input = AnyNode base_addr };
        set_ctrl field_addr;
        let addr =
            match offset with
            | Some (AnyData idx) ->
                let idx = convert_node memo g machine_g idx in
                let s =
                    match n.typ with
                    | Ptr p -> Types.get_size p
                    | _ -> assert false
                in
                let m = create_node (MulImm (Z.of_int s)) (AnyNode n) in
                G.add_node machine_g m { input = idx };
                set_ctrl m;
                let a = create_node Add (AnyNode n) in
                G.add_node machine_g a { lhs = AnyNode field_addr; rhs = AnyNode m };
                set_ctrl a;
                AnyNode a
            | None -> AnyNode field_addr
        in
        (* use set because the call to simple AddrOf already adds this key so we need to overwrite it *)
        Hashtbl.set memo ~key:(AnyNode n) ~data:addr;
        addr
    | Data Deref -> (
        match n.typ with
        | Struct _ ->
            (* struct dereference isn't a real thing because structs need to be accessed by ptr anyway *)
            let { Node.mem; ptr } = Node.G.get_dependencies_exn g n in
            let (AnyData ptr) = Option.value_exn ptr in
            let ptr = convert_node memo g machine_g ptr in
            Hashtbl.set memo ~key:(AnyNode n) ~data:ptr;
            ptr
        | _ ->
            simple Deref n (fun { mem; ptr } ->
                let (AnyMem mem) = Option.value_exn mem in
                let mem = convert_node memo g machine_g mem in
                let (AnyData ptr) = Option.value_exn ptr in
                let ptr = convert_node memo g machine_g ptr in
                { mem; ptr }))

and of_ctrl_node : type a.
    (Node.any, any) Hashtbl.t ->
    Node.G.readonly Node.G.t ->
    G.readwrite G.t ->
    (a, Node.ctrl) Node.t ->
    any =
   fun memo g machine_g n ->
    let set_ctrl : type a b. (a, b) t -> unit =
       fun node ->
        let ctrl =
            Node.G.get_ctrl g n
            |> Option.map ~f:(fun (AnyNode ctrl) -> convert_node memo g machine_g ctrl)
        in
        match ctrl with
        | None -> ()
        | Some (AnyNode ctrl) -> G.set_ctrl machine_g node ctrl
    in
    let simple : type a b t. a kind -> (b, t) Node.t -> (b -> a) -> any =
       fun kind n f ->
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let inputs = Node.G.get_dependencies_exn g n in
        G.add_node machine_g node (f inputs);
        set_ctrl node;
        AnyNode node
    in
    match n.kind with
    | Ctrl If -> (
        let { Node.input = cond } = Node.G.get_dependencies_exn g n in
        let (AnyData cond) = Option.value_exn cond in
        let op =
            match cond.kind with
            | Data Eq -> Some Eq
            | Data NEq -> Some NEq
            | Data Lt -> Some Lt
            | Data LEq -> Some LEq
            | Data Gt -> Some Gt
            | Data GEq -> Some GEq
            | _ -> None
        in
        match op with
        | Some op ->
            let kind = Jmp op in
            let node = create_node kind (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let cond = convert_node memo g machine_g cond in
            G.add_node machine_g node { input = cond };
            set_ctrl node;
            AnyNode node
        | None ->
            (* Predicate is not a compare. Add a compare node to compare the
               predicate value to 0 and set that as the depedency of the Jmp
               node *)
            let cmp_node = create_node (CmpImm Z.zero) (AnyNode n) in
            let kind = Jmp NEq in
            let node = create_node kind (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let cond = convert_node memo g machine_g cond in
            G.add_node machine_g cmp_node { input = cond };
            set_ctrl cmp_node;
            G.add_node machine_g node { input = AnyNode cmp_node };
            set_ctrl node;
            AnyNode node)
    | Ctrl Stop ->
        let (AnyNode node) = G.get_stop machine_g in
        let node = unpack_exn node (Ideal Stop) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.mem } : Node.stop = Node.G.get_dependencies_exn g n in
        let (AnyMem mem) = Option.value_exn mem in
        let mem = convert_node memo g machine_g mem in
        G.set_node_inputs machine_g node { mem };
        set_ctrl node;
        AnyNode node
    | Ctrl Start ->
        let node = G.get_start machine_g in
        node
    | Ctrl (Proj i) ->
        let kind = Ideal (CProj i) in

        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.input } = Node.G.get_dependencies_exn g n in
        let (AnyNode input) = Option.value_exn input in
        let inputs = { input = convert_node memo g machine_g input } in
        G.add_node machine_g node inputs;
        let (AnyNode ctrl) = inputs.input in
        G.set_ctrl machine_g node ctrl;
        AnyNode node
    | Ctrl Loop ->
        simple (Ideal Loop) n (fun { entry; backedge } ->
            let (AnyCtrl entry) = Option.value_exn entry in
            let (AnyCtrl backedge) = Option.value_exn backedge in
            {
              entry = convert_node memo g machine_g entry;
              backedge = convert_node memo g machine_g backedge;
            })
    | Ctrl Region ->
        simple (Ideal Region) n (fun { ctrl_inputs } ->
            let ctrl_inputs =
                List.map ctrl_inputs
                  ~f:(Option.map ~f:(fun (Node.AnyCtrl n) -> convert_node memo g machine_g n))
            in
            { ctrl_inputs })
    | Ctrl (Function { ret = _; signature = _; idx }) ->
        let node = create_node (FunctionProlog idx) (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.call_sites } = Node.G.get_dependencies_exn g n in
        let call_sites =
            List.map call_sites
              ~f:(Option.map ~f:(fun (Node.AnyCtrl call) -> convert_node memo g machine_g call))
        in
        G.add_node machine_g node { ctrl_inputs = call_sites };
        set_ctrl node;
        AnyNode node
    | Ctrl Return ->
        simple Return n (fun { mem; data } ->
            let (AnyMem mem) = Option.value_exn mem in
            let (AnyData data) = Option.value_exn data in
            {
              mem = convert_node memo g machine_g mem;
              value = convert_node memo g machine_g data;
              callee_saves = [];
            })
    | Ctrl FunctionCall ->
        let { Node.fun_ptr; mem; args } = Node.G.get_dependencies_exn g n in
        let (AnyData fun_ptr) = Option.value_exn fun_ptr in
        let (AnyMem mem) = Option.value_exn mem in
        let fun_idx = Types.get_fun_idx fun_ptr.typ in
        let kind = FunctionCall fun_idx in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let fun_ptr =
            if Option.is_some fun_idx then None else Some (convert_node memo g machine_g fun_ptr)
        in
        let mem = convert_node memo g machine_g mem in
        let args =
            List.map args ~f:(fun o -> Option.value_exn o)
            |> List.map ~f:(fun (Node.AnyData arg) -> convert_node memo g machine_g arg)
        in
        G.add_node machine_g node { fun_ptr; mem; args };
        set_ctrl node;
        AnyNode node
    | Ctrl FunctionCallEnd ->
        simple FunctionCallEnd n (fun { ret_nodes } ->
            let ret_nodes =
                List.map ret_nodes ~f:(fun o -> Option.value_exn o)
                |> List.map ~f:(fun (Node.AnyCtrl ret) -> convert_node memo g machine_g ret)
            in
            { ret_nodes })

and of_mem_node : type a.
    (Node.any, any) Hashtbl.t ->
    Node.G.readonly Node.G.t ->
    G.readwrite G.t ->
    (a, Node.mem) Node.t ->
    any =
   fun memo g machine_g n ->
    let set_ctrl : type a b. (a, b) t -> unit =
       fun node ->
        let ctrl =
            Node.G.get_ctrl g n
            |> Option.map ~f:(fun (AnyNode ctrl) -> convert_node memo g machine_g ctrl)
        in
        match ctrl with
        | None -> ()
        | Some (AnyNode ctrl) -> G.set_ctrl machine_g node ctrl
    in
    let simple : type a b t. a kind -> (b, t) Node.t -> (b -> a) -> any =
       fun kind n f ->
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let inputs = Node.G.get_dependencies_exn g n in
        G.add_node machine_g node (f inputs);
        set_ctrl node;
        AnyNode node
    in
    match n.kind with
    | Mem New ->
        simple New n (fun { mem; size } ->
            let (AnyMem mem) = Option.value_exn mem in
            let (AnyData size) = Option.value_exn size in
            { mem = convert_node memo g machine_g mem; size = convert_node memo g machine_g size })
    | Mem (Store _) ->
        (* TODO check for ops like add that can address memory directly *)
        let { Node.mem = _; ptr = _; value } = Node.G.get_dependencies_exn g n in
        let (AnyData value) = Option.value_exn value in
        let size = Types.get_size value.typ in
        assert (size <= 8);
        simple Store n (fun { mem; ptr; value } ->
            let (AnyMem mem) = Option.value_exn mem in
            let (AnyData ptr) = Option.value_exn ptr in
            let (AnyData value) = Option.value_exn value in
            {
              mem = convert_node memo g machine_g mem;
              ptr = convert_node memo g machine_g ptr;
              value = convert_node memo g machine_g value;
            })
    | Mem Copy ->
        let inputs = Node.G.get_dependencies_exn g n in
        let (AnyData src) = Option.value_exn inputs.src in
        let size =
            match src.typ with
            | Ptr p -> Types.get_size p
            | _ -> assert false
        in
        simple (RepMov size) n (fun { mem; src; dst } ->
            let (AnyMem mem) = Option.value_exn mem in
            let mem = convert_node memo g machine_g mem in
            let (AnyData src) = Option.value_exn src in
            let src = convert_node memo g machine_g src in
            let (AnyData dst) = Option.value_exn dst in
            let dst = convert_node memo g machine_g dst in
            { mem; src; dst })
    | Mem Phi ->
        let kind = Ideal Phi in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node.AnyMem n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node
    | Mem Param ->
        let kind = Param (-1) in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node.phi_inputs } = Node.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node.AnyMem n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node
    | Mem (Proj i) ->
        simple (Ideal (MProj i)) n (fun { input } ->
            let (AnyNode input) = Option.value_exn input in
            { input = convert_node memo g machine_g input })

and convert_node : type a t.
    (Node.any, any) Hashtbl.t -> Node.G.readonly Node.G.t -> G.readwrite G.t -> (a, t) Node.t -> any
    =
   fun memo g machine_g n ->
    match Hashtbl.find memo (AnyNode n) with
    | Some mn -> mn
    | None ->
        let res =
            match n.kind with
            | Data k ->
                let n = Node.as_data_exn n in
                of_data_node memo g machine_g n
            | Ctrl k ->
                let n = Node.as_ctrl_exn n in
                of_ctrl_node memo g machine_g n
            | Mem k ->
                let n = Node.as_mem_exn n in
                of_mem_node memo g machine_g n
            | Scope _ -> assert false
            | ForwardRef _ -> assert false
        in
        assert (Hashtbl.mem memo (AnyNode n));
        res

type change =
    | InputChange : ('a, 'b) t * 'a -> change
    | CtrlChange : ('a, 'b) t * ('a, unit) t -> change

let post_process machine_g =
    (* when changing a node's dependency we need to add a temp node that depends on the new_dep to make sure it doesn't get removed for not having any dependants. E.g. A jmp removes it's depedendancy on a set and set's it to the cmp directly. But the set might get removed if it has no dependants which in turn might remove the cmp for not having dependants *)
    let temp_node =
        create_node (Int Z.zero)
          (AnyNode (Node.create_data { filename = ""; line = 0; col = 0 } Types.ANY Constant))
    in
    G.fold machine_g ~init:[] ~f:(fun acc (AnyNode n) ->
        match n.kind with
        | Jmp op -> (
            let { input = AnyNode input } = G.get_dependencies_exn machine_g n in
            match input.kind with
            | Set op' -> (
                let { input = AnyNode set_input } = G.get_dependencies_exn machine_g input in
                match set_input.kind with
                | Cmp
                | CmpImm _ ->
                    assert (Poly.equal op op');
                    let new_input = { input = AnyNode set_input } in
                    InputChange (n, new_input) :: acc
                | _ -> acc)
            | Cmp
            | CmpImm _ ->
                acc
            | _ -> assert false
            (* match cmp_dep with *)
            (* | None -> acc *)
            (* | Some (_, cmp_n) -> *)
            (*     let set_idx, set_n = Option.value_exn set_dep in *)
            (*     let op = *)
            (*         match set_n.kind with *)
            (*         | Set op -> op *)
            (*         | _ -> assert false *)
            (*     in *)
            (*     n.kind <- Jmp op; *)
            (*     (n, set_n, set_idx, cmp_n) :: acc) *))
        | _ -> acc)
    |> List.iter ~f:(function
      | InputChange (n, new_input) -> G.set_node_inputs machine_g n new_input
      | CtrlChange (n, new_ctrl) -> G.set_ctrl machine_g n new_ctrl);
    (* it might be the case that there were no nodes to post process, in that
       case temp_node isn't added to the graph. This is fine *)
    try G.remove_node machine_g temp_node with
    | _ -> ()

let convert_graph g =
    let start = create_node (Ideal Start) (Node.G.get_start g) in
    let stop = create_node (Ideal Stop) (Node.G.get_stop g) in

    let machine_g = G.create ~start:(AnyNode start) ~stop:(AnyNode stop) in
    let memo = Hashtbl.create ~size:(Node.G.get_num_nodes g) (module Node.Any) in
    Hashtbl.add_exn memo ~key:(Node.G.get_start g) ~data:(AnyNode start);
    let (AnyNode stop_ir) = stop.ir_node in
    convert_node memo g machine_g stop_ir |> ignore;
    post_process machine_g;
    machine_g

module Any = struct
  type t = any

  let compare (AnyNode a) (AnyNode b) = Int.compare a.id b.id
  let equal (AnyNode a) (AnyNode b) = Int.equal a.id b.id
  let hash (AnyNode a) = Int.hash a.id
  let sexp_of_t = sexp_of_any
end
