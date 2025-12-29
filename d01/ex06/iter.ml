let rec iter (f : (int -> int)) (x : int) (n : int) : int =
    if n < 0 then
        -1
    else if n = 0 then
        x
    else
        iter f (f x) (n - 1)

(* tests *)
let main () =
    print_int (iter (fun x -> x * x) 2 (-42)); print_newline ();
    print_int (iter (fun x -> x * x) 2 4); print_newline ();
    print_int (iter (fun x -> x * 2) 2 4); print_newline ();
    print_int (iter (fun x -> x + 1) 0 10); print_newline ()

let _ = main ()
