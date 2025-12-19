let ft_print_alphabet () : unit =
    let z = int_of_char 'z' in

    let rec loop c =
        if c > z then
            print_newline ()
        else (
            print_char (char_of_int c);
            loop (c + 1)
        )
    in loop (int_of_char 'a')

(* tests *)
let _ = ft_print_alphabet ()
