let ft_print_comb () : unit =
    let print_number a b c =
        print_int a;
        print_int b;
        print_int c;
        if a != 7 then print_string ", "
    in

    let rec iterate_c a b c =
        if c < 10 then (
            print_number a b c;
            iterate_c a b (c + 1)
        )
    in

    let rec iterate_b a b =
        if b < 9 then (
            (iterate_c a b (b + 1);
            iterate_b a (b + 1))
        )
    in

    let rec iterate_a a =
        if a < 8 then (
            iterate_b a (a + 1);
            iterate_a (a + 1)
        )
    in iterate_a 0;

    print_string "\n"
 

(* tests *)
let _ = ft_print_comb ()
