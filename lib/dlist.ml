type 'a t = 'a list -> 'a list

let of_list l = fun tail -> l @ tail
let to_list dl = dl []
let singleton x = fun tail -> x :: tail
let empty () = fun tail -> tail
let append dl1 dl2 = fun tail -> dl1 (dl2 tail)
let push_back x dl = fun tail -> dl (x :: tail)
let concat dlists = List.fold_left (fun acc dl -> append acc dl) (empty ()) dlists
let ( @ ) = append
