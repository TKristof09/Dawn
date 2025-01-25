open Dawn.Dlist

let print_dl lst =
    let rec aux = function
        | [] -> ""
        | [ x ] -> string_of_int x
        | x :: xs -> string_of_int x ^ "; " ^ aux xs
    in
    print_string "[";
    print_string (lst |> to_list |> aux);
    print_string "]"

let%expect_test "empty list" =
    empty () |> print_dl;
    [%expect {| [] |}]

let%expect_test "singleton list" =
    singleton 1 |> print_dl;
    [%expect {| [1] |}]

let%expect_test "singleton list 2" =
    empty () |> push_back 1 |> print_dl;
    [%expect {| [1] |}]

let%expect_test "of_list and to_list roundtrip" =
    [ 1; 2; 3 ] |> of_list |> print_dl;
    [%expect {| [1; 2; 3] |}]

let%expect_test "multiple push_backs" =
    empty () |> push_back 1 |> push_back 2 |> push_back 3 |> print_dl;
    [%expect {| [1; 2; 3] |}]

let%expect_test "append empty lists" =
    let l1 = empty () in
    let l2 = empty () in
    append l1 l2 |> print_dl;
    [%expect {| [] |}]

let%expect_test "append with empty list" =
    let l1 = of_list [ 1; 2 ] in
    let l2 = empty () in
    append l1 l2 |> print_dl;
    [%expect {| [1; 2] |}]

let%expect_test "append two non-empty lists" =
    let l1 = of_list [ 1; 2 ] in
    let l2 = of_list [ 3; 4 ] in
    append l1 l2 |> print_dl;
    [%expect {| [1; 2; 3; 4] |}]

let%expect_test "multiple appends" =
    let l1 = of_list [ 1 ] in
    let l2 = of_list [ 2 ] in
    let l3 = of_list [ 3 ] in
    append l1 (append l2 l3) |> print_dl;
    [%expect {| [1; 2; 3] |}]

let%expect_test "mix of push_back and append" =
    let l1 = of_list [ 3; 4 ] in
    let l2 = of_list [ 5; 6 ] in
    let l = empty () |> push_back 1 |> push_back 2 in
    let l = append l l1 in
    append l l2 |> print_dl;
    [%expect {| [1; 2; 3; 4; 5; 6] |}]

let%expect_test "concat empty list of lists" =
    concat [] |> print_dl;
    [%expect {| [] |}]

let%expect_test "concat multiple lists" =
    concat [ of_list [ 1; 2 ]; of_list [ 3; 4 ]; of_list [ 5; 6 ] ] |> print_dl;
    [%expect {| [1; 2; 3; 4; 5; 6] |}]
