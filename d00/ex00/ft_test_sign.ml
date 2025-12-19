let ft_test_sign (n : int) : unit =
    if n >= 0 then
        print_endline "positive"
    else
        print_endline "negative"


(* tests *)
let _ = ft_test_sign 42
let _ = ft_test_sign 0
let _ = ft_test_sign (-42)
