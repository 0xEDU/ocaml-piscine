let ft_print_comb () = 
    let print_number x y z =
        print_int x;
        print_int y;
        print_int z;
        if x != 7 || y != 8 || z != 9 then print_string ", "
    in

    let rec loop2 x y z =
        if z != 10 then (print_number x y z; loop2 x y (z + 1))
    in

    let rec loop1 x y =
        if y != 9 then (loop2 x y 2; loop1 x (y + 1))
    in

    let rec loop0 x =
        if x != 8 then (loop1 x 1; loop0 (x + 1))
    in loop0 0;

    print_string "\n"

(* tests *)
let _ = ft_print_comb()
