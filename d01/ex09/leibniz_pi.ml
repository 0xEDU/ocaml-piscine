let leibniz_pi (delta: float) : int =
    if delta < 0. then -1 else

    let reference = 4. *. atan 1. in

    let rec compute k acc sign =
        let computed_value = 4. *. acc in
        let diff = reference -. computed_value in

        if diff <= delta && diff >= -.delta then
            k
        else
            compute
                (k + 1)
                (acc +. sign /. (2. *. float_of_int k +. 1.))
                (-.sign)
    in
    compute 0 0.0 1.0

(* tests *)
let main () =
    print_int (leibniz_pi (-1.)); print_newline();
    print_int (leibniz_pi 0.00002); print_newline();
    print_int (leibniz_pi 0.000000002); print_newline();
    print_int (leibniz_pi 0.000000000011); print_newline();
    print_int (leibniz_pi 0.00987); print_newline();
    print_int (leibniz_pi 0.1234); print_newline()

let _ = main ()
