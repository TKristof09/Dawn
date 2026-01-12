open Dawn

(* let test_str = *)
(*     {| *)
(*     let i:int = 0; *)
(*     let sum:int = 0; *)
(*     let c: int = 5; *)
(*     while(i == c - 1) { *)
(*         i = i + 1; *)
(*         let j:int = 0; *)
(*         while(i+j == 11){ *)
(*             //sum = j; *)
(*             j = j + 2; *)
(*         } *)
(*     } *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let i:int = 0; *)
(*     let c: int = 5; *)
(*     let sum: int = 1; *)
(*     while(i == c - 1) { *)
(*         let j:int = 0; *)
(*         i = i + 1; *)
(*         j = j + 2; *)
(*         sum = sum + j; *)
(*     } *)
(*     if(sum == 1) { *)
(*     } *)
(*     |} *)

(* let test_str = *)
(*     {| *)
(*     let i:int = 0; *)
(*     while(i == 1) { *)
(*         i = i + 1; *)
(*         let j:int = 0; *)
(*         while(j == 10){ *)
(*             j = j + 2; *)
(*         } *)
(*     } *)
(*     |} *)
(* let test_str = *)
(*     {| *)
(*     let x:int = 0; *)
(*     let y:int = 1; *)
(*     while(x == 0) { *)
(*         let tmp:int = x + y; *)
(*         y = x; *)
(*         x = tmp; *)
(*     } *)
(*     //if(y==1){ *)
(*     //    x = 1; *)
(*     //} *)
(*     |} *)

let test_str =
    {|
    let k:int = 1;
    let i:int = k;
    let j:int = i / 69;
    if(i == j) {j = j+1;}
    if(j==0){j=j+4;}

    |}

let () =
    match Parser.parse_str test_str with
    | Ok ast ->
        let g = Son.of_ast ast in
        (* Ir_printer.to_dot_machine g |> Printf.printf "\n\n%s\n" *)
        ignore g
    | Error msg -> Printf.eprintf "%s\n" msg
