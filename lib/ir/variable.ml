open Core

type 'node t = {
    name : string;
    node : 'node;
    is_const : bool;
    is_forward_ref : bool;
    idx : int;
  }
[@@deriving sexp_of]
