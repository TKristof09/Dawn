open Dawn

let test_str =
    {|
    let i:int = 0;
    let sum:int = 0;
    let c: int = 5;
    while(i == c - 1) {
        i = i + 1;
        let j:int = 0;
        while(j == 11){
            sum = sum + j;
            j = j + 2;
        }
    }
    |}

(*let test_str =*)
(*    {|*)
(*    let i:int = 0;*)
(*    let arg:int = 69;*)
(*    if(arg == 3){*)
(*        i = arg + 2;*)
(*    }*)
(*    i + 1;*)
(*    |}*)

let () =
    match Parser.parse_str test_str with
    | Ok ast ->
        let g = Son.of_ast ast in
        Ir_printer.to_dot_machine g |> Printf.printf "\n\n%s\n"
    | Error msg -> Printf.eprintf "%s\n" msg
