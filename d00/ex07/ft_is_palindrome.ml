let ft_is_palindrome (subject : string) =
    let rec check left right =
        if left < right then
            String.get subject left = String.get subject right
            && check (left + 1) (right - 1)
        else
            true
    in check 0 (String.length subject - 1)

(* tests *)
let print_bool expr =
    if expr then
        print_endline "true"
    else
        print_endline "false"

let _ = ft_is_palindrome "radar" |> print_bool (* should be true *)
let _ = ft_is_palindrome "" |> print_bool      (* should be true *)
let _ = ft_is_palindrome "osso" |> print_bool  (* should be true *)
let _ = ft_is_palindrome "121" |> print_bool   (* should be true *)
let _ = ft_is_palindrome "car" |> print_bool   (* should be false *)
let _ = ft_is_palindrome "bone" |> print_bool  (* should be false *)
let _ = ft_is_palindrome "1234" |> print_bool  (* should be false *)
