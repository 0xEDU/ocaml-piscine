let rec converges (f : ('a -> 'a)) (x: 'a) (n : int) : bool =
    let result = f x in

    if x = result then
        true
    else if n = 0 then
        false
    else
        converges f result (n - 1)

(* tests *)
let main () =
    let print_bool value =
        if value then
            print_endline "true"
        else
            print_endline "false"
    in

    print_bool (converges (( * ) 2) 2 5); (* same as (fun x -> 2 * x) *)
    print_bool (converges (fun x -> x / 2) 2 3);
    print_bool (converges (fun x -> x / 2) 2 2);
    print_bool (converges (fun x -> x * x) 1 2)

let _ = main ()
