let crossover (a : 'a list) (b : 'a list) : 'a list =
    let rec contains needle haystack =
        match haystack with
        | [] -> false
        | head :: tail when head = needle -> true
        | head :: tail -> contains needle tail
    in

    let rec loop a b acc =
        match a with
        | [] -> acc
        | a_head :: a_tail ->
                if contains a_head b then
                    loop a_tail b (a_head :: acc)
                else
                    loop a_tail b acc
    in
    loop a b []

(* tests *)
let main () =
    let rec print_list print_s l =
        match l with
        | [] -> print_endline "list is empty!"
        | _ ->
            let rec print_loop = function
                | x :: xs -> print_s x; print_char ' '; print_loop xs
                | [] -> ()
            in
            print_loop l;
            print_newline()
    in

    print_list print_string (crossover ["aaaa"] ["bbbb"; "aaaa"]);
    print_list print_int (crossover [123] [123; 456]);
    print_list print_int (crossover [123; 456] [123]);
    print_list print_int (crossover [123; 456; 789] [123; 456]);
    print_list print_string (crossover ["jjjj"] ["bbbb"; "aaaa"]); (* should print empty *)
    print_list print_string (crossover [] ["bbbb"; "aaaa"]); (* should print empty *)
    print_list print_string (crossover [] ["bbbb"; "aaaa"]) (* should print empty *)

let _ = main()
