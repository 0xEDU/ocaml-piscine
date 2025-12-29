let fibonacci (n : int) =
    let rec calculate n prev1 prev2 =
        if n < 0 then
            -1
        else if n = 0 || n = 1 then
            prev2
        else
            calculate (n - 1) (prev1 + prev2) prev1
    in
    calculate n 1 1

(* tests *)
let main () =
    print_int (fibonacci 1); print_newline ();
    print_int (fibonacci 5); print_newline ();
    print_int (fibonacci 9); print_newline ();
    print_int (fibonacci 23); print_newline ()

let _ = main ()
