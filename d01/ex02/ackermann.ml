let rec ackermann (m : int) (n : int) : int =
    if m = 0 then
        n + 1
    else if m > 0 && n = 0 then
        ackermann (m - 1) 1
    else if m > 0 && n > 0 then
        ackermann (m - 1) (ackermann m (n - 1))
    else
        -1

(* tests *)
let main () =
    print_int (ackermann (-1) (-1)); print_newline ();
    print_int (ackermann (-1) 1); print_newline ();
    print_int (ackermann 1 (-1)); print_newline ();
    print_int (ackermann 0 0); print_newline ();
    print_int (ackermann 2 3); print_newline ();
    print_int (ackermann 4 1); print_newline ()
    (*;print_int (ackermann 4 2); print_newline ()*) (* turns out that this would take more than the universe lifetime xD *)

let _ = main ()
