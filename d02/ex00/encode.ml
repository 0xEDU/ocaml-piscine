let encode (l : 'a list) : (int * 'a) list =
    let rec loop l n acc =
        match l with
        | head :: next :: tail when head = next -> loop (next :: tail) (n + 1) acc
        | head :: next :: tail when head != next -> loop (next :: tail) 1 (acc @ [(n, head)])
        | head :: [] -> loop [] 1 (acc @ [(n, head)])
        | _ -> acc
    in
    loop l 1 []

(* tests *)
let main () =
    let print_tuple print_s (i, s) =
        print_int i;
        print_char ' ';
        print_s s
    in

    let rec print_list print_s l =
        match l with
        | x :: xs -> print_tuple print_s x;
                     print_newline ();
                     print_list print_s xs
        | [] -> ()
    in

    print_list print_string (encode ["aaaa"; "bbbb"; "aaaa"]); print_newline ();
    print_list print_string (encode ["aaaa"; "aaaa"; "aaaa"]); print_newline ();
    print_list print_string (encode ["aaaa"]); print_newline ();
    print_list print_int (encode [123; 123; 123; 456]); print_newline ();
    print_list print_string (encode []); print_newline () (* should output empty *)

let _ = main()
