let ft_print_comb2 () : unit =
    let print_number a b c d =
        print_int a;
        print_int b;
        print_string " ";
        print_int c;
        print_int d;
        if b != 8 then print_string ", "
    in

    let rec iterate_d a b c d =
        if d < 10 then (
            print_number a b c d;
            iterate_d a b c (d + 1)
        )
    in

    let rec iterate_c a b c =
        if c < 10 then (
            iterate_d a b c 0;
            iterate_c a b (c + 1)
        )
    in

    let rec iterate_b a b =
        if b < 10 then (
            iterate_c a b 0;
            iterate_b a (b + 1)
        )
    in

    let rec iterate_a a =
        if a < 10 then (
            iterate_b a 0;
            iterate_a (a + 1)
        )
    in iterate_a 0;

    print_string "\n"
 

(* tests *)
let _ = ft_print_comb2 ()
