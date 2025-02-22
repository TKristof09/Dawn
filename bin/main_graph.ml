open Dawn

let test_str =
    {|
    let j: int = 0;  
    let x: int = 5;
    if(j == 10){
        if(x == 5){
            j = x + 10;
        }else{
            j = -1;
        }
    }
    else {
        j = 2;
    }
    |}

let () =
    match Parser.parse_str test_str with
    | Ok ast ->
        let g = Son.of_ast ast in
        Ir_printer.to_dot g |> Printf.printf "\n\n%s\n"
    | Error msg -> Printf.eprintf "%s\n" msg
