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

  type any = AnyNode : 'a t -> any

  and _ ideal =
      | Loop : loop ideal
      | CProj : int -> unary ideal
      | Start : unit ideal
      | Stop : stop ideal
      | Region : merge_point ideal
      | Phi : phi ideal
      | External : string -> unit ideal

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
      | FunctionProlog : int -> unit kind
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

  and 'a t = {
      id : int;
      mutable kind : 'a kind;
      ir_node : Node2.any;
      list_of_inputs : 'a -> any option list;
      inputs_of_list : any option list -> 'a;
    }
  [@@deriving sexp_of]

  let id n = n.id
  let equal a b = a.id = b.id
  let list_of_inputs n inps = n.list_of_inputs inps
  let inputs_of_list n l = n.inputs_of_list l

  let[@inline] type_eq : type a b. a t -> b t -> ((a, b) Type.eq * (unit, unit) Type.eq) option =
     fun a b -> _
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

let is_cheap_to_clone : type a. a t -> bool =
   fun n ->
    match n.kind with
    | Int _ -> true
    | Ptr -> true
    | Cmp -> true
    | CmpImm _ -> true
    | _ -> false

let is_control_node : type a. a t -> bool =
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

let is_blockhead : type a. a t -> bool =
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

let is_two_address : type a. a t -> bool =
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

let get_in_reg_mask : type a. G.readonly G.t -> a t -> int -> Registers.Mask.t option =
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
        (* skip memory input, known function call so no fun_ptr as input which makes memory at index 0 *)
        if i = 0 then
          None
        else
          Some (Registers.Mask.x64_systemv (i - 1))
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

let rec get_out_reg_mask : type a. G.readonly G.t -> a t -> int -> Registers.Mask.t option =
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
        if idx = 0 then
          None
        else
          Some (Registers.Mask.x64_systemv (idx - 1))
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

let get_register_kills : type a. a t -> Registers.Mask.t option =
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

let create_node : type a. a kind -> Node2.any -> a t =
   fun kind ir_node ->
    {
      id = next_id ();
      kind;
      ir_node;
      list_of_inputs = get_list_of_inputs kind;
      inputs_of_list = get_inputs_of_list kind;
    }

let rec of_data_node : type a.
    (Node2.any, any) Hashtbl.t ->
    Node2.G.readonly Node2.G.t ->
    G.readwrite G.t ->
    (a, Node2.data) Node2.t ->
    any =
   fun memo g machine_g n ->
    let set_ctrl : type a. a t -> unit =
       fun node ->
        let ctrl =
            Node2.G.get_ctrl g n
            |> Option.map ~f:(fun (AnyNode ctrl) -> convert_node memo g machine_g ctrl)
        in
        G.set_ctrl machine_g node ctrl
    in
    let get_const (Node2.AnyData n) =
        if Types.is_constant n.typ && Z.fits_int32 (Types.get_integer_const_exn n.typ) then
          Some (Types.get_integer_const_exn n.typ)
        else
          None
    in
    let binop_commutative kind kind_imm (n : (Node2.binop, Node2.data) Node2.t) =
        let deps = Node2.G.get_dependencies_exn g n in
        let lhs = Option.value_exn deps.lhs in
        let rhs = Option.value_exn deps.rhs in
        match (get_const lhs, get_const rhs) with
        | None, None ->
            let node = create_node kind (Node2.AnyNode n) in
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
    let binop_non_commutative kind kind_imm (n : (Node2.binop, Node2.data) Node2.t) =
        let deps = Node2.G.get_dependencies_exn g n in
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
            let node = create_node kind (Node2.AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let (AnyData lhs) = Option.value_exn deps.lhs in
            let lhs = convert_node memo g machine_g lhs in
            let rhs = convert_node memo g machine_g rhs in
            G.add_node machine_g node { lhs; rhs };
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
        let { Node2.lhs; rhs } = Node2.G.get_dependencies_exn g n in
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
        let { Node2.input } = Node2.G.get_dependencies_exn g n in
        let (AnyData input) = Option.value_exn input in
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
        let as_binop : type a b. (a, b) Node2.t -> (Node2.binop, Node2.data) Node2.t =
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
        let { Node2.lhs; rhs } = Node2.G.get_dependencies_exn g n in
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
        let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node2.AnyData n) -> convert_node memo g machine_g n))
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
        let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node2.AnyData n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node
    | Data (External name) ->
        let node = create_node (Ideal (External name)) (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        G.add_node machine_g node ();
        set_ctrl node;
        AnyNode node
    | Data Cast ->
        let { Node2.input } = Node2.G.get_dependencies_exn g n in
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

and of_ctrl_node : type a.
    (Node2.any, any) Hashtbl.t ->
    Node2.G.readonly Node2.G.t ->
    G.readwrite G.t ->
    (a, Node2.ctrl) Node2.t ->
    any =
   fun memo g machine_g n ->
    let set_ctrl : type a. a t -> unit =
       fun node ->
        let ctrl =
            Node2.G.get_ctrl g n
            |> Option.map ~f:(fun (AnyNode ctrl) -> convert_node memo g machine_g ctrl)
        in
        G.set_ctrl machine_g node ctrl
    in
    let simple : type a b t. a kind -> (b, t) Node2.t -> (b -> a) -> any =
       fun kind n f ->
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let inputs = Node2.G.get_dependencies_exn g n in
        G.add_node machine_g node (f inputs);
        set_ctrl node;
        AnyNode node
    in
    match n.kind with
    | Ctrl If -> (
        let { Node2.input = cond } = Node2.G.get_dependencies_exn g n in
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
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node2.mem } : Node2.stop = Node2.G.get_dependencies_exn g n in
        let (AnyMem mem) = Option.value_exn mem in
        let mem = convert_node memo g machine_g mem in
        G.set_node_inputs machine_g node { mem };
        set_ctrl node;
        AnyNode node
    | Ctrl Start ->
        let node = G.get_start machine_g in
        node
    | Ctrl (Proj i) ->
        simple (Ideal (CProj i)) n (fun { input } ->
            let (AnyCtrl input) = Option.value_exn input in
            { input = convert_node memo g machine_g input })
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
                  ~f:(Option.map ~f:(fun (Node2.AnyCtrl n) -> convert_node memo g machine_g n))
            in
            { ctrl_inputs })
    | Ctrl (Function { ret = _; signature = _; idx }) ->
        let node = create_node (FunctionProlog idx) (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node2.call_sites } = Node2.G.get_dependencies_exn g n in
        let call_sites =
            List.map call_sites
              ~f:(Option.map ~f:(fun (Node2.AnyCtrl call) -> convert_node memo g machine_g call))
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
        let { Node2.fun_ptr; mem; args } = Node2.G.get_dependencies_exn g n in
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
            |> List.map ~f:(fun (Node2.AnyData arg) -> convert_node memo g machine_g arg)
        in
        G.add_node machine_g node { fun_ptr; mem; args };
        set_ctrl node;
        AnyNode node
    | Ctrl FunctionCallEnd ->
        simple FunctionCallEnd n (fun { ret_nodes } ->
            let ret_nodes =
                List.map ret_nodes ~f:(fun o -> Option.value_exn o)
                |> List.map ~f:(fun (Node2.AnyCtrl ret) -> convert_node memo g machine_g ret)
            in
            { ret_nodes })

and of_mem_node : type a.
    (Node2.any, any) Hashtbl.t ->
    Node2.G.readonly Node2.G.t ->
    G.readwrite G.t ->
    (a, Node2.mem) Node2.t ->
    any =
   fun memo g machine_g n ->
    let set_ctrl : type a. a t -> unit =
       fun node ->
        let ctrl =
            Node2.G.get_ctrl g n
            |> Option.map ~f:(fun (AnyNode ctrl) -> convert_node memo g machine_g ctrl)
        in
        G.set_ctrl machine_g node ctrl
    in
    let simple : type a b t. a kind -> (b, t) Node2.t -> (b -> a) -> any =
       fun kind n f ->
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let inputs = Node2.G.get_dependencies_exn g n in
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
        let { Node2.mem = _; ptr = _; value } = Node2.G.get_dependencies_exn g n in
        let (AnyData value) = Option.value_exn value in
        let size = Types.get_size value.typ in
        assert (size <= 8);
        simple Store n (fun { mem; ptr; value } ->
            let (AnyMem mem) = Option.value_exn mem in
            let (AnyMem ptr) = Option.value_exn ptr in
            let (AnyData value) = Option.value_exn value in
            {
              mem = convert_node memo g machine_g mem;
              ptr = convert_node memo g machine_g ptr;
              value = convert_node memo g machine_g value;
            })
    | Mem (Load _) -> (
        (* TODO check for ops like add that can address memory directly *)
        match n.typ with
        | Struct _ ->
            let node = create_node AddrOf (AnyNode n) in
            Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
            let { Node2.mem = _; ptr } : Node2.load = Node2.G.get_dependencies_exn g n in
            let (AnyMem ptr) = Option.value_exn ptr in
            let ptr = convert_node memo g machine_g ptr in
            G.add_node machine_g node { input = ptr };
            set_ctrl node;
            AnyNode node
        | _ ->
            simple Load n (fun { Node2.mem; ptr } ->
                let (AnyMem mem) = Option.value_exn mem in
                let (AnyMem ptr) = Option.value_exn ptr in
                { mem = convert_node memo g machine_g mem; ptr = convert_node memo g machine_g ptr })
        )
    | Mem AddrOf -> (
        let node = create_node AddrOf (AnyNode n) in
        let { Node2.place; offset } = Node2.G.get_dependencies_exn g n in
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
    | Mem (AddrOfField f) ->
        let { Node2.place; offset } = Node2.G.get_dependencies_exn g n in
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
    | Mem Deref -> (
        match n.typ with
        | Struct _ ->
            (* struct dereference isn't a real thing because structs need to be accessed by ptr anyway *)
            let { Node2.mem; ptr } = Node2.G.get_dependencies_exn g n in
            let (AnyMem ptr) = Option.value_exn ptr in
            let ptr = convert_node memo g machine_g ptr in
            Hashtbl.set memo ~key:(AnyNode n) ~data:ptr;
            ptr
        | _ ->
            simple Deref n (fun { mem; ptr } ->
                let (AnyMem mem) = Option.value_exn mem in
                let mem = convert_node memo g machine_g mem in
                let (AnyMem ptr) = Option.value_exn ptr in
                let ptr = convert_node memo g machine_g ptr in
                { mem; ptr }))
    | Mem Copy ->
        let inputs = Node2.G.get_dependencies_exn g n in
        let (AnyMem src) = Option.value_exn inputs.src in
        let size =
            match src.typ with
            | Ptr p -> Types.get_size p
            | _ -> assert false
        in
        simple (RepMov size) n (fun { mem; src; dst } ->
            let (AnyMem mem) = Option.value_exn mem in
            let mem = convert_node memo g machine_g mem in
            let (AnyMem src) = Option.value_exn src in
            let src = convert_node memo g machine_g src in
            let (AnyMem dst) = Option.value_exn dst in
            let dst = convert_node memo g machine_g dst in
            { mem; src; dst })
    | Mem Phi ->
        let kind = Ideal Phi in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node2.AnyMem n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node
    | Mem Param ->
        let kind = Param 0 in
        let node = create_node kind (AnyNode n) in
        Hashtbl.add_exn memo ~key:(AnyNode n) ~data:(AnyNode node);
        let { Node2.phi_inputs } = Node2.G.get_dependencies_exn g n in
        let phi_inputs =
            List.map phi_inputs
              ~f:(Option.map ~f:(fun (Node2.AnyMem n) -> convert_node memo g machine_g n))
        in
        G.add_node machine_g node { phi_inputs };
        set_ctrl node;
        AnyNode node

and convert_node : type a t.
    (Node2.any, any) Hashtbl.t ->
    Node2.G.readonly Node2.G.t ->
    G.readwrite G.t ->
    (a, t) Node2.t ->
    any =
   fun memo g machine_g n ->
    match Hashtbl.find memo (AnyNode n) with
    | Some mn -> mn
    | None ->
        let res =
            match n.kind with
            | Data k ->
                let n = Node2.as_data_exn n in
                of_data_node memo g machine_g n
            | Ctrl k ->
                let n = Node2.as_ctrl_exn n in
                of_ctrl_node memo g machine_g n
            | Mem k ->
                let n = Node2.as_mem_exn n in
                of_mem_node memo g machine_g n
            | Scope _ -> assert false
            | ForwardRef _ -> assert false
        in
        assert (Hashtbl.mem memo (AnyNode n));
        res

let find_dep machine_g n ~f =
    Graph.get_dependencies machine_g n
    |> List.findi ~f:(fun _ dep ->
        match dep with
        | None -> false
        | Some dep -> f dep)
    |> Option.map ~f:(fun (i, n) -> (i, Option.value_exn n))

type change =
    | InputChange : 'a t * 'a -> change
    | CtrlChange : 'a t * 'a t -> change

let post_process machine_g =
    (* when changing a node's dependency we need to add a temp node that depends on the new_dep to make sure it doesn't get removed for not having any dependants. E.g. A jmp removes it's depedendancy on a set and set's it to the cmp directly. But the set might get removed if it has no dependants which in turn might remove the cmp for not having dependants *)
    let temp_node =
        create_node (Int Z.zero)
          (AnyNode (Node2.create_data { filename = ""; line = 0; col = 0 } Types.ANY Constant))
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
    (* |> List.iter ~f:(fun (node, old_dep, idx, new_dep) -> *)
    (*     Graph.add_dependencies machine_g temp_node [ Some new_dep ]; *)
    (*     Graph.remove_dependency machine_g ~node ~dep:old_dep; *)
    (*     Graph.set_dependency machine_g node (Some new_dep) idx); *)
    (* it might be the case that there were no nodes to post process, in that
       case temp_node isn't added to the graph. This is fine *)
    try Graph.remove_node machine_g temp_node with
    | _ -> ()

let convert_graph g =
    let start = create_node (Ideal Start) (Node2.G.get_start g) in
    let stop = create_node (Ideal Stop) (Node2.G.get_stop g) in

    let machine_g = G.create ~start:(AnyNode start) ~stop:(AnyNode stop) in
    let memo = Hashtbl.create ~size:(Node2.G.get_num_nodes g) (module Node2.Any) in
    Hashtbl.add_exn memo ~key:(Node2.G.get_start g) ~data:(AnyNode start);
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
