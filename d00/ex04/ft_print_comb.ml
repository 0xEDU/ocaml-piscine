let ft_print_comb () : unit =
    let print_number x y z =
        print_int x;
        print_int y;
        print_int z;
        if x != 7 then print_string ", "
    in

    let rec for_each_z x y z =
        if z < 10 then (print_number x y z; for_each_z x y (z + 1))
    in

    let rec for_each_y x y =
        if y < 9 then (for_each_z x y (y + 1); for_each_y x (y + 1))
    in

    let rec for_each_x x =
        if x < 8 then (for_each_y x (x + 1); for_each_x (x + 1))
    in for_each_x 0;

    print_string "\n"
 

(* tests *)
let _ = ft_print_comb()
