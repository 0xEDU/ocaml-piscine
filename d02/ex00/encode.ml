let encode (l : 'a list) : (int * 'a) list =
    (* using the @ operator in the main loop results in O(n^2) *)
    (* using :: and then reversing the list boils it down to O(n)  *)
    let rev l =
        let rec rev_loop l acc =
            match l with
            | [] -> acc
            | head :: tail -> rev_loop tail (head :: acc)
        in
        rev_loop l []
    in

    let rec encode_loop current count rest acc =
        match rest with
        | head :: tail when head = current -> encode_loop current (count + 1) tail acc
        | head :: tail -> encode_loop head 1 tail ((count, current) :: acc)
        | [] -> (count, current) :: acc
    in

    match l with
    | [] -> []
    | head :: tail -> rev (encode_loop head 1 tail [])

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
