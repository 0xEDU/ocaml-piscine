let rec ft_power (base : int) (exponent : int) =
    if exponent = 0 then
        1
    else if exponent = 1 then
        base
    else
        ft_power (base * base) (exponent / 2)


(* tests *)
let _ = ft_power 2 4 |> print_int; print_newline ()
let _ = ft_power 3 0 |> print_int; print_newline ()
let _ = ft_power 0 5 |> print_int; print_newline ()
let _ = ft_power 2 63 |> print_int; print_newline ()


