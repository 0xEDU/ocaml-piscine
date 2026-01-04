let sequence (n : int) : string =
    if n < 0 then "" else

    let base_case = [1] in

    let rec fold_left f acc sequence =
        match sequence with
        | [] -> acc
        | h :: t -> fold_left f (f acc h) t
    in

    let count (output, current_num, size) c =
        if current_num <> c then
            (output @ [size; current_num], c, 1)
        else
            (output, current_num, size + 1)
    in

    let rec iterate sequence n =
        if n > 0 then
            match sequence with
            | h :: _ -> let (output, num, size) = fold_left count ([], h, 0) sequence in
                        iterate (output @ [size; num]) (n - 1)
            | [] -> sequence
        else
            sequence
    in

    fold_left (fun acc n -> acc ^ string_of_int n) "" (iterate base_case n)

(* tests *)
let main () =
    print_string "Sequence for n = -1: "; print_endline (sequence (-1));
    print_string "Sequence for n = 0: "; print_endline (sequence 0);
    print_string "Sequence for n = 1: "; print_endline (sequence 1);
    print_string "Sequence for n = 2: "; print_endline (sequence 2);
    print_string "Sequence for n = 6: "; print_endline (sequence 6);
    print_string "Sequence for n = 10: "; print_endline (sequence 10)

let _ = main ()
