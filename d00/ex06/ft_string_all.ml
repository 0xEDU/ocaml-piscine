let ft_string_all (predicate : (char -> bool)) (subject : string) : bool =
    let rec check pos =
        if pos > String.length subject - 1 then
            true
        else
            predicate (String.get subject pos) && check (pos + 1)
    in check 0

(* tests *)
let print_bool expr =
    if expr then
        print_endline "true"
    else
        print_endline "false"

let is_digit c = c >= '0' && c <= '9'
let is_char c = c >= 'a' && c <= 'z'

let _ = ft_string_all is_digit "1234" |> print_bool                      (* should be true *)
let _ = ft_string_all is_char "abcdefghijklmnopqrstuvwxyz" |> print_bool (* should be true *)
let _ = ft_string_all is_digit "1a234" |> print_bool                     (* should be false *)
let _ = ft_string_all is_char "1234a" |> print_bool                      (* should be false *)

