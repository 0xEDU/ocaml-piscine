let gray (n : int) =
    if n < 0 then () else

    let rec get_bits x width acc =
        if width = 0 then
            acc
        else
            get_bits
                (x lsr 1)
                (width - 1)
                (string_of_int (x land 1) ^ acc)
    in

    let max_combinations = 1 lsl n in

    let rec print_gray i =
        if i < max_combinations then (
            let gray = i lxor (i lsr 1) in (* Gray code formula: i ^ (i >> 1) *)

            print_string (get_bits gray n "");
            print_char ' ';
            print_gray (i + 1)
        )
    in
    print_gray 0;
    print_newline ()

let main () =
    print_string "Gray sequence for 0 bits: "; gray 0;
    print_string "Gray sequence for 1 bits: "; gray 1;
    print_string "Gray sequence for 2 bits: "; gray 2;
    print_string "Gray sequence for 3 bits: "; gray 3;
    print_string "Gray sequence for 4 bits: "; gray 4

let _ = main ()
