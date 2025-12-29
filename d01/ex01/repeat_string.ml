let rec repeat_string ?(str = "x") (n : int) : string =
    if n < 0 then
        "Error"
    else if n = 0 then
        ""
    else
        str ^ repeat_string ~str (n - 1)

(* tests *)
let main () =
    print_endline (repeat_string 0);
    print_endline (repeat_string ~str:"foo" 0);
    print_endline (repeat_string (-1));
    print_endline (repeat_string ~str:"foo" 1);
    print_endline (repeat_string 10);
    print_endline (repeat_string ~str:"bar" 10)

let _ = main ()
