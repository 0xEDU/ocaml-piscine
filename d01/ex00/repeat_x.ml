let rec repeat_x (n : int) : string =
    if n < 0 then
        "Error"
    else if n = 0 then
        ""
    else
        "x" ^ repeat_x (n - 1)

(* tests *)
let main () =
    print_endline (repeat_x (-1));
    print_endline (repeat_x 0);
    print_endline (repeat_x 1);
    print_endline (repeat_x 10);
    print_endline (repeat_x 120)

let _ = main ()
