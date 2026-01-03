let sequence (n : int) : string =
    if n < 0 then "" else

    let base = "1" in

    let count (output, current_char, size) c =
        if current_char <> c then
            (output ^ string_of_int size ^ String.make 1 current_char, c, 1)
        else
            (output, current_char, size + 1)
    in

    let rec iterate sequence n =
        if n > 0 then
            let (output, c, size) = String.fold_left count ("", sequence.[0], 0) sequence in
            iterate (output ^ string_of_int size ^ String.make 1 c) (n - 1)
        else
            sequence
    in
    iterate base n

(* tests *)
let main () =
    print_string "Sequence for n = -1: "; print_endline (sequence (-1));
    print_string "Sequence for n = 0: "; print_endline (sequence 0);
    print_string "Sequence for n = 1: "; print_endline (sequence 1);
    print_string "Sequence for n = 2: "; print_endline (sequence 2);
    print_string "Sequence for n = 3: "; print_endline (sequence 3);
    print_string "Sequence for n = 4: "; print_endline (sequence 4);
    print_string "Sequence for n = 5: "; print_endline (sequence 5);
    print_string "Sequence for n = 6: "; print_endline (sequence 6)

let _ = main ()
