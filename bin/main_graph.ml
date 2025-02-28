open Dawn

let test_str =
    {|
    let a:int = 1;
    let b:int  = 2;
    while(a == 10) {
        if (a == 2){
            a = 3;
        }
        else {
            b = 4;
        }
    }
    |}

let () =
    match Parser.parse_str test_str with
    | Ok ast ->
        let g = Son.of_ast ast in
        Ir_printer.to_dot g |> Printf.printf "\n\n%s\n"
    | Error msg -> Printf.eprintf "%s\n" msg
