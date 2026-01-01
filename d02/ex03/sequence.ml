let sequence (n : int) : string =
    if n < 0 then "" else

    let base_sequence = [1] in

    let rec loop n acc =
        if n = 0 then
            acc
        else
            loop (n - 1) base_sequence
    in
    loop n base_sequence


(* tests *)
let main () =
    print_endline (sequence (-4)); (* should print nothing *)
    print_endline (sequence 4)

let _ = main ()
