let ft_print_comb2 () : unit =
    let print_number a b =
        if a < 10 then print_int 0;
        print_int a;

        print_string " ";

        if b < 10 then print_int 0;
        print_int b;

        if a < 98 then print_string ", "
    in

    let rec iterate_b a b =
        if b < 100 then (
            print_number a b;
            iterate_b a (b + 1)
        )
    in

    let rec iterate_a a =
        if a < 99 then (
            iterate_b a (a + 1);
            iterate_a (a + 1)
        )
    in iterate_a 0;

    print_string "\n"
 

(* tests *)
let _ = ft_print_comb2 ()
