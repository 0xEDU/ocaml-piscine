let gray (n : int) =
    let rec print_bit n =
        if n > 1 then print_bit (n lsr 1);
        print_int (n land 1)
    in

    let max_combinations = 1 lsl n in
    let rec print_gray_bits n =
        if n < max_combinations then
            (print_bit (n lxor (n lsr 1)); (* Gray code formula: i XOR (i / 2) *)
            print_char ' ';
            print_gray_bits (n + 1))
    in
    print_gray_bits 0;
    print_newline ()

let main () =
    gray 3

let _ = main ()
