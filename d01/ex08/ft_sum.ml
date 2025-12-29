let ft_sum (i : (int -> float)) (m : int) (n : int) : float =
    if n < m then nan else

    let rec sum k acc =
        if k > n then
            acc
        else
            sum (k + 1) (acc +. (i k))
    in
    sum m 0.

(* tests *)
let main () =
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10); print_newline ();
    print_float (ft_sum (fun i -> float_of_int (i * i)) 3 6); print_newline ();
    print_float (ft_sum (fun i -> float_of_int i) 1 10); print_newline () (* equals (10 * (10 + 1)) / 2, which is 55. *)

let _ = main ()
